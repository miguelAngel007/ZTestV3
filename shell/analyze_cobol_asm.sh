#!/bin/sh
# Lista programas ASM llamados desde programas COBOL
# Busca CALL en COBOL hacia programas que NO son COBOL (son ASM)
# Un programa es ASM si existe pero NO contiene "DIVISION"

PGMLIB="E.LIB.PROGRAM"
COPYLIB="E.LIB.COPY.COB"
TMPDIR="/u/idz/B45617/tmp"
PGM="IM31"

# Directorio de trabajo (donde esta el workspace)
WORKDIR="/u/idz/B45617/workspaceV3/ZTestV3"
ASM_DIR="$TMPDIR/files_asm"
DBB_BUILD_FILE="$TMPDIR/dbb_build_asm.sh"
DBB_LOG_FILE="$TMPDIR/dbb_build_asm.log"

# Archivos para tracking
PROCESSED_PGMS="$TMPDIR/processed_pgms.txt"
PROCESSED_COPY="$TMPDIR/processed_copy.txt"
FOUND_ASM="$TMPDIR/found_asm.txt"
FOUND_COPY="$TMPDIR/found_copy.txt"
RELATIONS="$TMPDIR/relations_asm.txt"
TREE="$TMPDIR/tree_asm.txt"
TMPFILE="$TMPDIR/current.tmp"

# Limpiar archivos previos
rm -f "$PROCESSED_PGMS" "$PROCESSED_COPY" "$FOUND_ASM" "$FOUND_COPY" "$RELATIONS" "$TREE"
rm -f "$TMPDIR"/node_*.lst
touch "$PROCESSED_PGMS" "$PROCESSED_COPY" "$FOUND_ASM" "$FOUND_COPY" "$RELATIONS" "$TREE"

echo "=== Buscando programas ASM llamados desde: $PGM ==="
echo ""

# Funcion para verificar si un miembro existe en un PDS
member_exists() {
    _pds=$1
    _member=$2
    cp "//'${_pds}(${_member})'" "$TMPDIR/check.tmp" 2>/dev/null
    _result=$?
    rm -f "$TMPDIR/check.tmp"
    return $_result
}

# Funcion para verificar si es COBOL (contiene DIVISION)
is_cobol() {
    _pds=$1
    _member=$2
    cp "//'${_pds}(${_member})'" "$TMPDIR/check_cobol.tmp" 2>/dev/null
    if [ $? -ne 0 ]; then
        rm -f "$TMPDIR/check_cobol.tmp"
        return 1
    fi
    grep -q "DIVISION" "$TMPDIR/check_cobol.tmp" 2>/dev/null
    _result=$?
    rm -f "$TMPDIR/check_cobol.tmp"
    return $_result
}

# Funcion para verificar si ya fue procesado
is_processed() {
    _name=$1
    grep -q "^${_name}$" "$PROCESSED_PGMS" 2>/dev/null
    return $?
}

# Funcion para marcar como procesado
mark_processed() {
    _name=$1
    echo "$_name" >> "$PROCESSED_PGMS"
}

# Funcion para imprimir arbol - version iterativa con stack
print_tree_iterative() {
    _root=$1
    _visited_file=$2
    
    # Stack: cada linea tiene formato "nodo|nivel|linea_actual|total_lineas"
    _stack="$TMPDIR/tree_stack.txt"
    
    # Inicializar con el nodo raiz
    grep "^${_root}|" "$RELATIONS" 2>/dev/null > "$TMPDIR/node_${_root}.lst"
    _root_total=`wc -l < "$TMPDIR/node_${_root}.lst" | tr -d ' '`
    
    if [ "$_root_total" -eq 0 ]; then
        return
    fi
    
    # Push inicial: nodo|nivel|linea_actual|total
    echo "${_root}|1|1|${_root_total}" > "$_stack"
    
    while [ -s "$_stack" ]; do
        # Pop: leer ultima linea del stack
        _current=`tail -1 "$_stack"`
        
        # Obtener componentes
        _node=`echo "$_current" | cut -d'|' -f1`
        _level=`echo "$_current" | cut -d'|' -f2`
        _line=`echo "$_current" | cut -d'|' -f3`
        _total=`echo "$_current" | cut -d'|' -f4`
        
        # Archivo de hijos de este nodo
        _node_file="$TMPDIR/node_${_node}.lst"
        
        # Si ya procesamos todas las lineas de este nodo, pop y continuar
        if [ "$_line" -gt "$_total" ]; then
            sed '$d' "$_stack" > "$TMPDIR/stack_tmp.txt"
            mv "$TMPDIR/stack_tmp.txt" "$_stack"
            rm -f "$_node_file"
            continue
        fi
        
        # Generar indentacion
        _indent=""
        _i=0
        while [ $_i -lt $_level ]; do
            _indent="${_indent}|   "
            _i=`expr $_i + 1`
        done
        
        # Obtener la linea actual del archivo de hijos
        _rel=`awk "NR==$_line" "$_node_file"`
        _child=`echo "$_rel" | cut -d'|' -f2`
        _child_type=`echo "$_rel" | cut -d'|' -f3`
        
        # Imprimir este hijo
        echo "${_indent}+-- [$_child_type] $_child"
        
        # Incrementar linea en el stack
        _new_line=`expr $_line + 1`
        sed '$d' "$_stack" > "$TMPDIR/stack_tmp.txt"
        echo "${_node}|${_level}|${_new_line}|${_total}" >> "$TMPDIR/stack_tmp.txt"
        mv "$TMPDIR/stack_tmp.txt" "$_stack"
        
        # Si es COBOL o CPY y no visitado, push sus hijos al stack
        if [ "$_child_type" = "COB" ] || [ "$_child_type" = "CPY" ]; then
            if ! grep -q "^${_child}$" "$_visited_file" 2>/dev/null; then
                echo "$_child" >> "$_visited_file"
                
                grep "^${_child}|" "$RELATIONS" 2>/dev/null > "$TMPDIR/node_${_child}.lst"
                _child_total=`wc -l < "$TMPDIR/node_${_child}.lst" | tr -d ' '`
                
                if [ "$_child_total" -gt 0 ]; then
                    _next_level=`expr $_level + 1`
                    echo "${_child}|${_next_level}|1|${_child_total}" >> "$_stack"
                fi
            fi
        fi
    done
    
    rm -f "$_stack" "$TMPDIR/stack_tmp.txt"
}

# Funcion para analizar un programa COBOL y extraer CALLs a ASM
analyze_program() {
    _pgm=$1
    
    # Verificar si ya fue procesado
    if is_processed "$_pgm"; then
        return
    fi
    mark_processed "$_pgm"
    
    # Verificar si existe y es COBOL
    if ! is_cobol "$PGMLIB" "$_pgm"; then
        return
    fi
    
    echo "Analizando COBOL: $_pgm"
    
    # Copiar a temporal
    cp "//'${PGMLIB}(${_pgm})'" "$TMPFILE"
    
    # Extraer CALL (solo nombre del programa)
    awk 'substr($0,7,1) != "*" && index($0," CALL ") {
        line = substr($0,8)
        if (match(line, /CALL ['\''"][A-Z0-9]+['\''"]/)) {
            call_part = substr(line, RSTART, RLENGTH)
            gsub(/CALL ['\''"]/, "", call_part)
            gsub(/['\''"]/, "", call_part)
            print call_part
        }
    }' "$TMPFILE" | sort -u > "$TMPDIR/calls.tmp"
    
    # Procesar CALLs encontrados
    while read _called; do
        if [ -n "$_called" ]; then
            # Verificar si existe
            if member_exists "$PGMLIB" "$_called"; then
                # Si NO es COBOL, es ASM
                if ! is_cobol "$PGMLIB" "$_called"; then
                    if ! grep -q "^${_called}$" "$FOUND_ASM" 2>/dev/null; then
                        echo "  -> ASM encontrado: $_called"
                        echo "$_called" >> "$FOUND_ASM"
                    fi
                    # Guardar relacion para el arbol
                    echo "${_pgm}|${_called}|ASM" >> "$RELATIONS"
                else
                    # Es COBOL, guardar relacion para seguir la cadena
                    echo "${_pgm}|${_called}|COB" >> "$RELATIONS"
                fi
            fi
        fi
    done < "$TMPDIR/calls.tmp"
    
    # Extraer COPY (nombre del copybook)
    awk 'substr($0,7,1) != "*" && index($0," COPY ") {
        line = substr($0,8)
        if (match(line, /COPY [A-Z0-9]+/)) {
            copy_part = substr(line, RSTART, RLENGTH)
            gsub(/COPY /, "", copy_part)
            gsub(/\..*/, "", copy_part)
            print copy_part
        }
    }' "$TMPFILE" | sort -u > "$TMPDIR/copies.tmp"
    
    # Registrar copybooks para analizar y guardar relacion
    while read _copybook; do
        if [ -n "$_copybook" ]; then
            if ! grep -q "^${_copybook}$" "$FOUND_COPY" 2>/dev/null; then
                echo "$_copybook" >> "$FOUND_COPY"
            fi
            # Guardar relacion programa -> copybook para el arbol
            echo "${_pgm}|${_copybook}|CPY" >> "$RELATIONS"
        fi
    done < "$TMPDIR/copies.tmp"
    
    rm -f "$TMPFILE" "$TMPDIR/calls.tmp" "$TMPDIR/copies.tmp"
}

# Funcion para analizar un copybook y extraer CALLs a ASM
analyze_copybook() {
    _cpy=$1
    
    # Verificar si ya fue procesado
    if grep -q "^${_cpy}$" "$PROCESSED_COPY" 2>/dev/null; then
        return
    fi
    echo "$_cpy" >> "$PROCESSED_COPY"
    
    # Verificar si existe
    if ! member_exists "$COPYLIB" "$_cpy"; then
        return
    fi
    
    echo "Analizando COPY: $_cpy"
    
    # Copiar a temporal
    cp "//'${COPYLIB}(${_cpy})'" "$TMPFILE"
    
    # Extraer CALL (solo nombre del programa)
    awk 'substr($0,7,1) != "*" && index($0," CALL ") {
        line = substr($0,8)
        if (match(line, /CALL ['\''"][A-Z0-9]+['\''"]/)) {
            call_part = substr(line, RSTART, RLENGTH)
            gsub(/CALL ['\''"]/, "", call_part)
            gsub(/['\''"]/, "", call_part)
            print call_part
        }
    }' "$TMPFILE" | sort -u > "$TMPDIR/calls.tmp"
    
    # Procesar CALLs encontrados
    while read _called; do
        if [ -n "$_called" ]; then
            # Verificar si existe
            if member_exists "$PGMLIB" "$_called"; then
                # Si NO es COBOL, es ASM
                if ! is_cobol "$PGMLIB" "$_called"; then
                    if ! grep -q "^${_called}$" "$FOUND_ASM" 2>/dev/null; then
                        echo "  -> ASM encontrado (en CPY): $_called"
                        echo "$_called" >> "$FOUND_ASM"
                    fi
                    # Guardar relacion para el arbol
                    echo "${_cpy}|${_called}|ASM" >> "$RELATIONS"
                fi
            fi
        fi
    done < "$TMPDIR/calls.tmp"
    
    # Extraer COPY anidados
    awk 'substr($0,7,1) != "*" && index($0," COPY ") {
        line = substr($0,8)
        if (match(line, /COPY [A-Z0-9]+/)) {
            copy_part = substr(line, RSTART, RLENGTH)
            gsub(/COPY /, "", copy_part)
            gsub(/\..*/, "", copy_part)
            print copy_part
        }
    }' "$TMPFILE" | sort -u > "$TMPDIR/nested.tmp"
    
    # Registrar copybooks anidados y guardar relacion
    while read _nested; do
        if [ -n "$_nested" ]; then
            if ! grep -q "^${_nested}$" "$FOUND_COPY" 2>/dev/null; then
                echo "$_nested" >> "$FOUND_COPY"
            fi
            # Guardar relacion copybook -> copybook anidado
            echo "${_cpy}|${_nested}|CPY" >> "$RELATIONS"
        fi
    done < "$TMPDIR/nested.tmp"
    
    rm -f "$TMPFILE" "$TMPDIR/calls.tmp" "$TMPDIR/nested.tmp"
}

# === INICIO DEL ANALISIS ===

# Analizar programa principal
analyze_program "$PGM"

# Loop para analizar programas COBOL encontrados (buscar mas ASM)
# Necesitamos lista de COBOL para seguir buscando
FOUND_COBOL="$TMPDIR/found_cobol_temp.txt"
rm -f "$FOUND_COBOL"
touch "$FOUND_COBOL"

# Re-analizar para encontrar COBOL llamados y seguir la cadena
ITERATION=0
MAX_ITER=50
while [ $ITERATION -lt $MAX_ITER ]; do
    ITERATION=`expr $ITERATION + 1`
    HAY_NUEVOS=0
    
    # Buscar COBOL llamados para seguir analizando
    cat "$PROCESSED_PGMS" > "$TMPDIR/pgms_done.tmp"
    
    while read _pgm_done; do
        if [ -n "$_pgm_done" ] && is_cobol "$PGMLIB" "$_pgm_done"; then
            cp "//'${PGMLIB}(${_pgm_done})'" "$TMPFILE" 2>/dev/null
            if [ $? -eq 0 ]; then
                awk 'substr($0,7,1) != "*" && index($0," CALL ") {
                    line = substr($0,8)
                    if (match(line, /CALL ['\''"][A-Z0-9]+['\''"]/)) {
                        call_part = substr(line, RSTART, RLENGTH)
                        gsub(/CALL ['\''"]/, "", call_part)
                        gsub(/['\''"]/, "", call_part)
                        print call_part
                    }
                }' "$TMPFILE" | sort -u > "$TMPDIR/calls.tmp"
                
                while read _called; do
                    if [ -n "$_called" ]; then
                        if ! is_processed "$_called"; then
                            if is_cobol "$PGMLIB" "$_called"; then
                                HAY_NUEVOS=1
                                analyze_program "$_called"
                            fi
                        fi
                    fi
                done < "$TMPDIR/calls.tmp"
            fi
        fi
    done < "$TMPDIR/pgms_done.tmp"
    
    # Procesar copybooks encontrados
    if [ -f "$FOUND_COPY" ]; then
        cat "$FOUND_COPY" > "$TMPDIR/copy_to_check.tmp"
        while read _c; do
            if [ -n "$_c" ]; then
                if ! grep -q "^${_c}$" "$PROCESSED_COPY" 2>/dev/null; then
                    HAY_NUEVOS=1
                    analyze_copybook "$_c"
                fi
            fi
        done < "$TMPDIR/copy_to_check.tmp"
    fi
    
    # Si no hay nuevos, salir
    if [ $HAY_NUEVOS -eq 0 ]; then
        break
    fi
done

rm -f "$TMPDIR/pgms_done.tmp" "$TMPDIR/copy_to_check.tmp" "$FOUND_COBOL" "$TMPFILE" "$TMPDIR/calls.tmp"
rm -f "$TMPDIR"/node_*.lst

# === ARBOL DE DEPENDENCIAS ===
echo "" >> "$TREE"
echo "=========================================" >> "$TREE"
echo "  ARBOL DE PROGRAMAS ASM" >> "$TREE"
echo "  (Solo muestra CALLs a ASM)" >> "$TREE"
echo "=========================================" >> "$TREE"
echo "[COB] $PGM" >> "$TREE"

# Crear archivo para tracking de nodos visitados
VISITED="$TMPDIR/visited_tree.txt"
rm -f "$VISITED"
touch "$VISITED"
echo "$PGM" >> "$VISITED"

# Imprimir arbol iterativo
print_tree_iterative "$PGM" "$VISITED" >> "$TREE"

rm -f "$VISITED"
rm -f "$TMPDIR"/node_*.lst
echo "=========================================" >> "$TREE"

# Mostrar arbol
cat "$TREE"
echo ""
echo "Arbol guardado en: $TREE"

# === RESUMEN ===
echo ""
echo "========================================="
echo "  PROGRAMAS ASM ENCONTRADOS"
echo "========================================="
if [ -s "$FOUND_ASM" ]; then
    sort -u "$FOUND_ASM"
else
    echo "  (ninguno)"
fi

echo ""
TOTAL_ASM=0
if [ -s "$FOUND_ASM" ]; then
    TOTAL_ASM=`sort -u "$FOUND_ASM" | wc -l | tr -d ' '`
fi
echo "========================================="
echo "Total programas ASM: $TOTAL_ASM"
echo "========================================="

# === COPIAR ARCHIVOS ASM ===
echo ""
echo "=== Copiando archivos ASM ==="

# Crear directorio si no existe
if [ ! -d "$ASM_DIR" ]; then
    mkdir -p "$ASM_DIR"
    echo "Directorio creado: $ASM_DIR"
fi

# Limpiar archivo de comandos DBB
rm -f "$DBB_BUILD_FILE"

# Crear script con cabecera
cat > "$DBB_BUILD_FILE" << 'HEADER'
#!/bin/sh
# Script generado automaticamente para compilar programas ASM
# Ejecutar: sh dbb_build_asm.sh

LOGFILE="/u/idz/B45617/tmp/dbb_build_asm.log"
rm -f "$LOGFILE"
touch "$LOGFILE"

OK_COUNT=0
FAIL_COUNT=0

echo "=== Inicio de compilacion ASM ===" | tee -a "$LOGFILE"
echo "Fecha: $(date)" | tee -a "$LOGFILE"
echo "" | tee -a "$LOGFILE"

HEADER

# Copiar cada archivo ASM y agregar comando al script
if [ -s "$FOUND_ASM" ]; then
    sort -u "$FOUND_ASM" | while read _asm; do
        if [ -n "$_asm" ]; then
            _dest="$ASM_DIR/${_asm}.asm"
            cp "//'${PGMLIB}(${_asm})'" "$_dest" 2>/dev/null
            if [ $? -eq 0 ]; then
                echo "  Copiado: $_asm -> $_dest"
                # Agregar comando al script con control de resultado
                cat >> "$DBB_BUILD_FILE" << EOF
echo "Compilando: $_asm" | tee -a "\$LOGFILE"
. /u/idz/B45617/profileV3 && \$DBB_HOME/bin/dbb build user $ASM_DIR/${_asm}.asm --hlq B45617.DBB --verbose --config '$WORKDIR/dbb-app.yaml' >> "\$LOGFILE" 2>&1
if [ \$? -eq 0 ]; then
    echo "  [OK] $_asm" | tee -a "\$LOGFILE"
    OK_COUNT=\`expr \$OK_COUNT + 1\`
else
    echo "  [FAIL] $_asm" | tee -a "\$LOGFILE"
    FAIL_COUNT=\`expr \$FAIL_COUNT + 1\`
fi
echo "" >> "\$LOGFILE"

EOF
            else
                echo "  ERROR copiando: $_asm"
            fi
        fi
    done
fi

# Agregar resumen al final del script
cat >> "$DBB_BUILD_FILE" << 'FOOTER'
echo "" | tee -a "$LOGFILE"
echo "=========================================" | tee -a "$LOGFILE"
echo "  RESUMEN DE COMPILACION" | tee -a "$LOGFILE"
echo "=========================================" | tee -a "$LOGFILE"
echo "Exitosos: $OK_COUNT" | tee -a "$LOGFILE"
echo "Fallidos: $FAIL_COUNT" | tee -a "$LOGFILE"
echo "=========================================" | tee -a "$LOGFILE"
echo "Log completo en: $LOGFILE"
FOOTER

chmod +x "$DBB_BUILD_FILE"

echo ""
echo "========================================="
echo "Archivos ASM copiados en: $ASM_DIR"
echo "Script de compilacion: $DBB_BUILD_FILE"
echo "Para ejecutar: sh $DBB_BUILD_FILE"
echo "========================================="

# === LIMPIEZA FINAL ===
rm -f "$TMPDIR"/node_*.lst
rm -f "$TMPDIR/tree_stack.txt" "$TMPDIR/stack_tmp.txt"
rm -f "$TMPDIR/check.tmp" "$TMPDIR/check_cobol.tmp"

#!/bin/sh
# Analisis completo de dependencias: COBOL, Copybooks y ASM
# Muestra todo en un solo arbol unificado
# Marca programas no encontrados con "- not found"

PGMLIB="E.LIB.PROGRAM"
COPYLIB="E.LIB.COPY.COB"
TMPDIR="/u/idz/B45617/sh_opt"
PGM="IM31"

# Directorio de trabajo (donde esta el workspace)
WORKDIR="/u/idz/B45617/dbb_workspace/ZTestV3"

# Directorios para copiar archivos
COBOL_DIR="$TMPDIR/cobol"
COPY_DIR="$TMPDIR/copybook"
ASM_DIR="$TMPDIR/asm"

# Archivos para tracking
PROCESSED_PGMS="$TMPDIR/processed_pgms.txt"
PROCESSED_COPY="$TMPDIR/processed_copy.txt"
FOUND_COBOL="$TMPDIR/found_cobol.txt"
FOUND_COPY="$TMPDIR/found_copy.txt"
FOUND_ASM="$TMPDIR/found_asm.txt"
NOT_FOUND="$TMPDIR/not_found.txt"
RELATIONS="$TMPDIR/relations_all.txt"
TREE="$TMPDIR/tree_all.txt"
TMPFILE="$TMPDIR/current.tmp"

# Limpiar archivos previos
rm -f "$PROCESSED_PGMS" "$PROCESSED_COPY" "$FOUND_COBOL" "$FOUND_COPY" "$FOUND_ASM" "$NOT_FOUND" "$RELATIONS" "$TREE"
rm -f "$TMPDIR"/node_*.lst
touch "$PROCESSED_PGMS" "$PROCESSED_COPY" "$FOUND_COBOL" "$FOUND_COPY" "$FOUND_ASM" "$NOT_FOUND" "$RELATIONS" "$TREE"

echo "=== Analizando dependencias completas de: $PGM ==="
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

# Funcion para verificar si ya fue procesado (programa)
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
        _child_status=`echo "$_rel" | cut -d'|' -f4`
        
        # Imprimir este hijo con status si no encontrado
        if [ "$_child_status" = "NF" ]; then
            echo "${_indent}+-- [$_child_type] $_child - not found"
        else
            echo "${_indent}+-- [$_child_type] $_child"
        fi
        
        # Incrementar linea en el stack
        _new_line=`expr $_line + 1`
        sed '$d' "$_stack" > "$TMPDIR/stack_tmp.txt"
        echo "${_node}|${_level}|${_new_line}|${_total}" >> "$TMPDIR/stack_tmp.txt"
        mv "$TMPDIR/stack_tmp.txt" "$_stack"
        
        # Si es COBOL o CPY y no visitado y existe, push sus hijos al stack
        if [ "$_child_type" = "COB" ] || [ "$_child_type" = "CPY" ]; then
            if [ "$_child_status" != "NF" ]; then
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
        fi
    done
    
    rm -f "$_stack" "$TMPDIR/stack_tmp.txt"
}

# Funcion para analizar un programa COBOL
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
    
    # Agregar a encontrados
    if ! grep -q "^${_pgm}$" "$FOUND_COBOL" 2>/dev/null; then
        echo "$_pgm" >> "$FOUND_COBOL"
    fi
    
    echo "Analizando COBOL: $_pgm"
    
    # Copiar a temporal
    cp "//'${PGMLIB}(${_pgm})'" "$TMPFILE"
    
    # Extraer CALL (solo nombre del programa)
    awk 'substr($0,7,1) != "*" && index($0," CALL ") {
        line = substr($0,8)
        if (match(line, /CALL +['\''"]+[A-Z0-9]+['\''"]/)) {
            call_part = substr(line, RSTART, RLENGTH)
            gsub(/CALL +['\''"]+/, "", call_part)
            gsub(/['\''"]/, "", call_part)
            print call_part
        }
    }' "$TMPFILE" | sort -u > "$TMPDIR/calls.tmp"
    
    # Procesar CALLs encontrados
    while read _called; do
        if [ -n "$_called" ]; then
            # Verificar si existe
            if member_exists "$PGMLIB" "$_called"; then
                # Si es COBOL
                if is_cobol "$PGMLIB" "$_called"; then
                    if ! grep -q "^${_called}$" "$FOUND_COBOL" 2>/dev/null; then
                        echo "$_called" >> "$FOUND_COBOL"
                    fi
                    # Guardar relacion: padre|hijo|tipo|status (OK=existe)
                    echo "${_pgm}|${_called}|COB|OK" >> "$RELATIONS"
                else
                    # Es ASM
                    if ! grep -q "^${_called}$" "$FOUND_ASM" 2>/dev/null; then
                        echo "  -> ASM: $_called"
                        echo "$_called" >> "$FOUND_ASM"
                    fi
                    echo "${_pgm}|${_called}|ASM|OK" >> "$RELATIONS"
                fi
            else
                # No existe - registrar como not found
                if ! grep -q "^${_called}$" "$NOT_FOUND" 2>/dev/null; then
                    echo "  -> NOT FOUND: $_called"
                    echo "$_called" >> "$NOT_FOUND"
                fi
                # Asumir que es ASM si no existe (no podemos verificar)
                echo "${_pgm}|${_called}|ASM|NF" >> "$RELATIONS"
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
    
    # Registrar copybooks
    while read _copybook; do
        if [ -n "$_copybook" ]; then
            if member_exists "$COPYLIB" "$_copybook"; then
                if ! grep -q "^${_copybook}$" "$FOUND_COPY" 2>/dev/null; then
                    echo "$_copybook" >> "$FOUND_COPY"
                fi
                echo "${_pgm}|${_copybook}|CPY|OK" >> "$RELATIONS"
            else
                if ! grep -q "^${_copybook}$" "$NOT_FOUND" 2>/dev/null; then
                    echo "  -> CPY NOT FOUND: $_copybook"
                    echo "$_copybook" >> "$NOT_FOUND"
                fi
                echo "${_pgm}|${_copybook}|CPY|NF" >> "$RELATIONS"
            fi
        fi
    done < "$TMPDIR/copies.tmp"
    
    rm -f "$TMPFILE" "$TMPDIR/calls.tmp" "$TMPDIR/copies.tmp"
}

# Funcion para analizar un copybook
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
        if (match(line, /CALL +['\''"][A-Z0-9]+['\''"]/)) {
            call_part = substr(line, RSTART, RLENGTH)
            gsub(/CALL +['\''"]/, "", call_part)
            gsub(/['\''"]/, "", call_part)
            print call_part
        }
    }' "$TMPFILE" | sort -u > "$TMPDIR/calls.tmp"
    
    # Procesar CALLs encontrados
    while read _called; do
        if [ -n "$_called" ]; then
            if member_exists "$PGMLIB" "$_called"; then
                if is_cobol "$PGMLIB" "$_called"; then
                    if ! grep -q "^${_called}$" "$FOUND_COBOL" 2>/dev/null; then
                        echo "$_called" >> "$FOUND_COBOL"
                    fi
                    echo "${_cpy}|${_called}|COB|OK" >> "$RELATIONS"
                else
                    if ! grep -q "^${_called}$" "$FOUND_ASM" 2>/dev/null; then
                        echo "  -> ASM (en CPY): $_called"
                        echo "$_called" >> "$FOUND_ASM"
                    fi
                    echo "${_cpy}|${_called}|ASM|OK" >> "$RELATIONS"
                fi
            else
                if ! grep -q "^${_called}$" "$NOT_FOUND" 2>/dev/null; then
                    echo "  -> NOT FOUND (en CPY): $_called"
                    echo "$_called" >> "$NOT_FOUND"
                fi
                echo "${_cpy}|${_called}|ASM|NF" >> "$RELATIONS"
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
    
    # Registrar copybooks anidados
    while read _nested; do
        if [ -n "$_nested" ]; then
            if member_exists "$COPYLIB" "$_nested"; then
                if ! grep -q "^${_nested}$" "$FOUND_COPY" 2>/dev/null; then
                    echo "$_nested" >> "$FOUND_COPY"
                fi
                echo "${_cpy}|${_nested}|CPY|OK" >> "$RELATIONS"
            else
                if ! grep -q "^${_nested}$" "$NOT_FOUND" 2>/dev/null; then
                    echo "  -> CPY NOT FOUND: $_nested"
                    echo "$_nested" >> "$NOT_FOUND"
                fi
                echo "${_cpy}|${_nested}|CPY|NF" >> "$RELATIONS"
            fi
        fi
    done < "$TMPDIR/nested.tmp"
    
    rm -f "$TMPFILE" "$TMPDIR/calls.tmp" "$TMPDIR/nested.tmp"
}

# === INICIO DEL ANALISIS ===

# Analizar programa principal
analyze_program "$PGM"

# Loop para analizar programas COBOL y copybooks encontrados
ITERATION=0
MAX_ITER=50
while [ $ITERATION -lt $MAX_ITER ]; do
    ITERATION=`expr $ITERATION + 1`
    HAY_NUEVOS=0
    
    # Procesar programas COBOL encontrados
    if [ -f "$FOUND_COBOL" ]; then
        cat "$FOUND_COBOL" > "$TMPDIR/cobol_to_check.tmp"
        while read _c; do
            if [ -n "$_c" ]; then
                if ! is_processed "$_c"; then
                    HAY_NUEVOS=1
                    analyze_program "$_c"
                fi
            fi
        done < "$TMPDIR/cobol_to_check.tmp"
    fi
    
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

rm -f "$TMPDIR/cobol_to_check.tmp" "$TMPDIR/copy_to_check.tmp" "$TMPFILE" "$TMPDIR/calls.tmp"
rm -f "$TMPDIR"/node_*.lst

# === ARBOL DE DEPENDENCIAS ===
echo "" >> "$TREE"
echo "=========================================" >> "$TREE"
echo "  ARBOL DE DEPENDENCIAS COMPLETO" >> "$TREE"
echo "  [COB]=COBOL [CPY]=Copybook [ASM]=Assembler" >> "$TREE"
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
echo "  RESUMEN"
echo "========================================="

TOTAL_COBOL=0
if [ -s "$FOUND_COBOL" ]; then
    TOTAL_COBOL=`sort -u "$FOUND_COBOL" | wc -l | tr -d ' '`
fi

TOTAL_COPY=0
if [ -s "$FOUND_COPY" ]; then
    TOTAL_COPY=`sort -u "$FOUND_COPY" | wc -l | tr -d ' '`
fi

TOTAL_ASM=0
if [ -s "$FOUND_ASM" ]; then
    TOTAL_ASM=`sort -u "$FOUND_ASM" | wc -l | tr -d ' '`
fi

TOTAL_NF=0
if [ -s "$NOT_FOUND" ]; then
    TOTAL_NF=`sort -u "$NOT_FOUND" | wc -l | tr -d ' '`
fi

echo "Programas COBOL: $TOTAL_COBOL"
echo "Copybooks:       $TOTAL_COPY"
echo "Programas ASM:   $TOTAL_ASM"
echo "No encontrados:  $TOTAL_NF"

if [ -s "$NOT_FOUND" ]; then
    echo ""
    echo "Archivos NO ENCONTRADOS:"
    sort -u "$NOT_FOUND" | while read _nf; do
        echo "  - $_nf"
    done
fi

echo "========================================="

# === COPIAR ARCHIVOS ===
echo ""
echo "=== Copiando archivos ==="

# Crear directorios si no existen
for _dir in "$COBOL_DIR" "$COPY_DIR" "$ASM_DIR"; do
    if [ ! -d "$_dir" ]; then
        mkdir -p "$_dir"
        echo "Directorio creado: $_dir"
    fi
done

# Copiar COBOL
echo ""
echo "Copiando COBOL..."
if [ -s "$FOUND_COBOL" ]; then
    sort -u "$FOUND_COBOL" | while read _cob; do
        if [ -n "$_cob" ]; then
            _dest="$COBOL_DIR/${_cob}.cbl"
            cp "//'${PGMLIB}(${_cob})'" "$_dest" 2>/dev/null
            if [ $? -eq 0 ]; then
                echo "  Copiado: $_cob -> $_dest"
            else
                echo "  ERROR copiando: $_cob"
            fi
        fi
    done
fi

# Copiar Copybooks
echo ""
echo "Copiando Copybooks..."
if [ -s "$FOUND_COPY" ]; then
    sort -u "$FOUND_COPY" | while read _cpy; do
        if [ -n "$_cpy" ]; then
            _dest="$COPY_DIR/${_cpy}.cpy"
            cp "//'${COPYLIB}(${_cpy})'" "$_dest" 2>/dev/null
            if [ $? -eq 0 ]; then
                echo "  Copiado: $_cpy -> $_dest"
            else
                echo "  ERROR copiando: $_cpy"
            fi
        fi
    done
fi

# Copiar ASM
echo ""
echo "Copiando ASM..."
if [ -s "$FOUND_ASM" ]; then
    sort -u "$FOUND_ASM" | while read _asm; do
        if [ -n "$_asm" ]; then
            _dest="$ASM_DIR/${_asm}.asm"
            cp "//'${PGMLIB}(${_asm})'" "$_dest" 2>/dev/null
            if [ $? -eq 0 ]; then
                echo "  Copiado: $_asm -> $_dest"
            else
                echo "  ERROR copiando: $_asm"
            fi
        fi
    done
fi

# === GENERAR SCRIPTS DBB ===
echo ""
echo "=== Generando scripts DBB ==="

DBB_BUILD_ASM="$TMPDIR/dbb_build_asm.sh"
DBB_BUILD_COBOL="$TMPDIR/dbb_build_cobol.sh"
COBOL_ORDER="$TMPDIR/cobol_order.txt"

rm -f "$DBB_BUILD_ASM" "$DBB_BUILD_COBOL" "$COBOL_ORDER"

# === SCRIPT ASM ===
cat > "$DBB_BUILD_ASM" << 'HEADER'
#!/bin/sh
# Script generado automaticamente para compilar programas ASM
# Ejecutar: sh dbb_build_asm.sh

LOGFILE="/u/idz/B45617/sh_opt/dbb_build_asm.log"
rm -f "$LOGFILE"
touch "$LOGFILE"

OK_COUNT=0
FAIL_COUNT=0

echo "=== Inicio de compilacion ASM ===" | tee -a "$LOGFILE"
echo "Fecha: $(date)" | tee -a "$LOGFILE"
echo "" | tee -a "$LOGFILE"

HEADER

if [ -s "$FOUND_ASM" ]; then
    sort -u "$FOUND_ASM" | while read _asm; do
        if [ -n "$_asm" ]; then
            cat >> "$DBB_BUILD_ASM" << EOF
echo "Compilando ASM: $_asm" | tee -a "\$LOGFILE"
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
        fi
    done
fi

cat >> "$DBB_BUILD_ASM" << 'FOOTER'
echo "" | tee -a "$LOGFILE"
echo "=========================================" | tee -a "$LOGFILE"
echo "  RESUMEN DE COMPILACION ASM" | tee -a "$LOGFILE"
echo "=========================================" | tee -a "$LOGFILE"
echo "Exitosos: $OK_COUNT" | tee -a "$LOGFILE"
echo "Fallidos: $FAIL_COUNT" | tee -a "$LOGFILE"
echo "=========================================" | tee -a "$LOGFILE"
echo "Log completo en: $LOGFILE"
FOOTER

chmod +x "$DBB_BUILD_ASM"

# === ORDENAR COBOL POR PROFUNDIDAD ===
# Profundidad = maxima cadena de CALLs a otros COBOL
# Profundidad 0 = no llama a ningun COBOL (compilar primero)
# Mayor profundidad = compilar al final

COBOL_DEPTH="$TMPDIR/cobol_depth.txt"
COBOL_CALLS="$TMPDIR/cobol_calls.txt"
rm -f "$COBOL_DEPTH" "$COBOL_CALLS"
touch "$COBOL_DEPTH" "$COBOL_CALLS"

# Extraer solo relaciones COBOL -> COBOL
grep "|COB|OK" "$RELATIONS" 2>/dev/null | while read _rel; do
    _caller=`echo "$_rel" | cut -d'|' -f1`
    _called=`echo "$_rel" | cut -d'|' -f2`
    # Solo si ambos son COBOL encontrados
    if grep -q "^${_caller}$" "$FOUND_COBOL" 2>/dev/null; then
        if grep -q "^${_called}$" "$FOUND_COBOL" 2>/dev/null; then
            echo "${_caller}|${_called}" >> "$COBOL_CALLS"
        fi
    fi
done

# Calcular profundidad iterativamente
# Inicializar: todos con profundidad -1 (no calculada)
sort -u "$FOUND_COBOL" | while read _cob; do
    echo "${_cob}|-1" >> "$COBOL_DEPTH"
done

# Iterar hasta que no haya cambios
_cambios=1
_iter=0
while [ $_cambios -eq 1 ] && [ $_iter -lt 20 ]; do
    _iter=`expr $_iter + 1`
    _cambios=0
    
    # Para cada COBOL
    sort -u "$FOUND_COBOL" | while read _cob; do
        # Obtener profundidad actual
        _current_depth=`grep "^${_cob}|" "$COBOL_DEPTH" | tail -1 | cut -d'|' -f2`
        
        # Contar cuantos COBOL llama este programa
        _num_calls=`grep "^${_cob}|" "$COBOL_CALLS" 2>/dev/null | wc -l | tr -d ' '`
        
        if [ "$_num_calls" -eq 0 ]; then
            # No llama a nadie -> profundidad 0
            if [ "$_current_depth" -ne 0 ]; then
                # Actualizar
                grep -v "^${_cob}|" "$COBOL_DEPTH" > "$TMPDIR/depth_tmp.txt"
                echo "${_cob}|0" >> "$TMPDIR/depth_tmp.txt"
                mv "$TMPDIR/depth_tmp.txt" "$COBOL_DEPTH"
                echo "1" > "$TMPDIR/cambios.txt"
            fi
        else
            # Calcular max profundidad de los que llama + 1
            _max_child=-1
            grep "^${_cob}|" "$COBOL_CALLS" 2>/dev/null | while read _call_rel; do
                _child=`echo "$_call_rel" | cut -d'|' -f2`
                _child_depth=`grep "^${_child}|" "$COBOL_DEPTH" | tail -1 | cut -d'|' -f2`
                if [ "$_child_depth" -ge 0 ] && [ "$_child_depth" -gt "$_max_child" ]; then
                    echo "$_child_depth" > "$TMPDIR/max_child.txt"
                fi
            done
            
            if [ -f "$TMPDIR/max_child.txt" ]; then
                _max_child=`cat "$TMPDIR/max_child.txt"`
                rm -f "$TMPDIR/max_child.txt"
                _new_depth=`expr $_max_child + 1`
                
                if [ "$_new_depth" -ne "$_current_depth" ]; then
                    grep -v "^${_cob}|" "$COBOL_DEPTH" > "$TMPDIR/depth_tmp.txt"
                    echo "${_cob}|${_new_depth}" >> "$TMPDIR/depth_tmp.txt"
                    mv "$TMPDIR/depth_tmp.txt" "$COBOL_DEPTH"
                    echo "1" > "$TMPDIR/cambios.txt"
                fi
            fi
        fi
    done
    
    if [ -f "$TMPDIR/cambios.txt" ]; then
        _cambios=1
        rm -f "$TMPDIR/cambios.txt"
    else
        _cambios=0
    fi
done

# Crear archivo de orden final
touch "$COBOL_ORDER"
cat "$COBOL_DEPTH" | while read _line; do
    _cob=`echo "$_line" | cut -d'|' -f1`
    _depth=`echo "$_line" | cut -d'|' -f2`
    # Si no se pudo calcular (-1), poner al final antes de PGM
    if [ "$_depth" -lt 0 ]; then
        _depth=98
    fi
    # Programa principal siempre al final
    if [ "$_cob" = "$PGM" ]; then
        _depth=99
    fi
    echo "${_depth}|${_cob}" >> "$COBOL_ORDER"
done

rm -f "$COBOL_DEPTH" "$COBOL_CALLS" "$TMPDIR/depth_tmp.txt"

# === SCRIPT COBOL ===
cat > "$DBB_BUILD_COBOL" << 'HEADER'
#!/bin/sh
# Script generado automaticamente para compilar programas COBOL
# Orden: dependencias primero, programa principal al final
# Ejecutar: sh dbb_build_cobol.sh

LOGFILE="/u/idz/B45617/sh_opt/dbb_build_cobol.log"
rm -f "$LOGFILE"
touch "$LOGFILE"

OK_COUNT=0
FAIL_COUNT=0

echo "=== Inicio de compilacion COBOL ===" | tee -a "$LOGFILE"
echo "Fecha: $(date)" | tee -a "$LOGFILE"
echo "" | tee -a "$LOGFILE"

HEADER

# Ordenar: menor profundidad primero (0=no llama a nadie), programa principal al final
if [ -s "$COBOL_ORDER" ]; then
    sort -t'|' -k1 -n "$COBOL_ORDER" | while read _line; do
        _cob=`echo "$_line" | cut -d'|' -f2`
        if [ -n "$_cob" ]; then
            cat >> "$DBB_BUILD_COBOL" << EOF
echo "Compilando COBOL: $_cob" | tee -a "\$LOGFILE"
. /u/idz/B45617/profileV3 && \$DBB_HOME/bin/dbb build user $COBOL_DIR/${_cob}.cbl --hlq B45617.DBB --verbose --config '$WORKDIR/dbb-app.yaml' >> "\$LOGFILE" 2>&1
if [ \$? -eq 0 ]; then
    echo "  [OK] $_cob" | tee -a "\$LOGFILE"
    OK_COUNT=\`expr \$OK_COUNT + 1\`
else
    echo "  [FAIL] $_cob" | tee -a "\$LOGFILE"
    FAIL_COUNT=\`expr \$FAIL_COUNT + 1\`
fi
echo "" >> "\$LOGFILE"

EOF
        fi
    done
fi

cat >> "$DBB_BUILD_COBOL" << 'FOOTER'
echo "" | tee -a "$LOGFILE"
echo "=========================================" | tee -a "$LOGFILE"
echo "  RESUMEN DE COMPILACION COBOL" | tee -a "$LOGFILE"
echo "=========================================" | tee -a "$LOGFILE"
echo "Exitosos: $OK_COUNT" | tee -a "$LOGFILE"
echo "Fallidos: $FAIL_COUNT" | tee -a "$LOGFILE"
echo "=========================================" | tee -a "$LOGFILE"
echo "Log completo en: $LOGFILE"
FOOTER

chmod +x "$DBB_BUILD_COBOL"

# Mostrar orden de compilacion COBOL
echo ""
echo "Orden de compilacion COBOL (por profundidad):"
if [ -s "$COBOL_ORDER" ]; then
    _orden=1
    sort -t'|' -k1 -n "$COBOL_ORDER" | while read _line; do
        _cob=`echo "$_line" | cut -d'|' -f2`
        _depth=`echo "$_line" | cut -d'|' -f1`
        echo "  $_orden. $_cob (nivel $_depth)"
        _orden=`expr $_orden + 1`
    done
fi

rm -f "$COBOL_ORDER"

echo ""
echo "========================================="
echo "Archivos copiados:"
echo "  COBOL:    $COBOL_DIR"
echo "  Copybook: $COPY_DIR"
echo "  ASM:      $ASM_DIR"
echo ""
echo "Scripts de compilacion:"
echo "  ASM:   $DBB_BUILD_ASM"
echo "  COBOL: $DBB_BUILD_COBOL"
echo ""
echo "Ejecutar:"
echo "  sh $DBB_BUILD_ASM"
echo "  sh $DBB_BUILD_COBOL"
echo "========================================="

# === LIMPIEZA FINAL ===
rm -f "$TMPDIR"/node_*.lst
rm -f "$TMPDIR/tree_stack.txt" "$TMPDIR/stack_tmp.txt"
rm -f "$TMPDIR/check.tmp" "$TMPDIR/check_cobol.tmp"

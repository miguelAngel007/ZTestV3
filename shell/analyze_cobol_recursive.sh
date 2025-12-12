#!/bin/sh
# Analisis recursivo de dependencias COBOL
# Solo analiza CALL a programas COBOL (no assembler)
# y COPY de copybooks
# Muestra arbol de dependencias (treeview)

PGMLIB="E.LIB.PROGRAM"
COPYLIB="E.LIB.COPY.COB"
TMPDIR="/u/idz/B45617/tmp"
PGM="IM31"

# Directorios para copiar archivos
COBOL_DIR="$TMPDIR/cobol"
COPY_DIR="$TMPDIR/copybook"

# Archivos para tracking
PROCESSED_PGMS="$TMPDIR/processed_pgms.txt"
PROCESSED_COPY="$TMPDIR/processed_copy.txt"
FOUND_PGMS="$TMPDIR/found_pgms.txt"
FOUND_COPY="$TMPDIR/found_copy.txt"
RELATIONS="$TMPDIR/relations.txt"
TREE="$TMPDIR/tree.txt"
TMPFILE="$TMPDIR/current.tmp"

# Limpiar archivos previos
rm -f "$PROCESSED_PGMS" "$PROCESSED_COPY" "$FOUND_PGMS" "$FOUND_COPY" "$RELATIONS" "$TREE"
touch "$PROCESSED_PGMS" "$PROCESSED_COPY" "$FOUND_PGMS" "$FOUND_COPY" "$RELATIONS" "$TREE"

echo "=== Analizando programa: $PGM ==="
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

# Funcion para generar indentacion del arbol
get_indent() {
    _level=$1
    _indent=""
    _i=0
    while [ $_i -lt $_level ]; do
        _indent="${_indent}|   "
        _i=`expr $_i + 1`
    done
    echo "$_indent"
}

# Funcion para imprimir arbol - version iterativa con stack
print_tree_iterative() {
    _root=$1
    _visited_file=$2
    
    # Stack: cada linea tiene formato "nodo|nivel|linea_actual|total_lineas"
    _stack="$TMPDIR/tree_stack.txt"
    
    # Inicializar con el nodo raiz
    # Obtener hijos del nodo raiz
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
            # Eliminar ultima linea del stack (pop)
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
        
        # Incrementar linea en el stack (actualizar ultima linea)
        _new_line=`expr $_line + 1`
        sed '$d' "$_stack" > "$TMPDIR/stack_tmp.txt"
        echo "${_node}|${_level}|${_new_line}|${_total}" >> "$TMPDIR/stack_tmp.txt"
        mv "$TMPDIR/stack_tmp.txt" "$_stack"
        
        # Si es programa y no visitado, push sus hijos al stack
        if [ "$_child_type" = "PGM" ]; then
            if ! grep -q "^${_child}$" "$_visited_file" 2>/dev/null; then
                echo "$_child" >> "$_visited_file"
                
                # Obtener hijos de este programa
                grep "^${_child}|" "$RELATIONS" 2>/dev/null > "$TMPDIR/node_${_child}.lst"
                _child_total=`wc -l < "$TMPDIR/node_${_child}.lst" | tr -d ' '`
                
                if [ "$_child_total" -gt 0 ]; then
                    # Push al stack
                    _next_level=`expr $_level + 1`
                    echo "${_child}|${_next_level}|1|${_child_total}" >> "$_stack"
                fi
            fi
        fi
    done
    
    rm -f "$_stack" "$TMPDIR/stack_tmp.txt"
}

# Funcion para verificar si ya fue procesado
is_processed() {
    _file=$1
    _name=$2
    grep -q "^${_name}$" "$_file" 2>/dev/null
    return $?
}

# Funcion para marcar como procesado
mark_processed() {
    _file=$1
    _name=$2
    echo "$_name" >> "$_file"
}

# Funcion para analizar un programa COBOL
analyze_program() {
    _pgm=$1
    _parent=$2
    
    # Verificar si ya fue procesado
    if is_processed "$PROCESSED_PGMS" "$_pgm"; then
        return
    fi
    mark_processed "$PROCESSED_PGMS" "$_pgm"
    
    # Verificar si existe y es COBOL
    if ! is_cobol "$PGMLIB" "$_pgm"; then
        # No es COBOL (es assembler o no existe), ignorar
        return
    fi
    
    echo "Analizando programa: $_pgm"
    
    # Copiar a temporal
    cp "//'${PGMLIB}(${_pgm})'" "$TMPFILE"
    
    # Extraer CALL (solo nombre del programa)
    awk 'substr($0,7,1) != "*" && index($0," CALL ") {
        line = substr($0,8)
        # Buscar CALL seguido de comilla simple o doble
        if (match(line, /CALL ['\''"][A-Z0-9]+['\''"]/)) {
            call_part = substr(line, RSTART, RLENGTH)
            # Extraer solo el nombre
            gsub(/CALL ['\''"]/, "", call_part)
            gsub(/['\''"]/, "", call_part)
            print call_part
        }
    }' "$TMPFILE" | sort -u > "$TMPDIR/calls.tmp"
    
    # Procesar CALLs encontrados
    while read _called; do
        if [ -n "$_called" ]; then
            # Verificar si es COBOL
            if is_cobol "$PGMLIB" "$_called"; then
                if ! grep -q "^${_called}$" "$FOUND_PGMS" 2>/dev/null; then
                    echo "$_called" >> "$FOUND_PGMS"
                fi
                # Guardar relacion: padre|hijo|tipo
                echo "${_pgm}|${_called}|PGM" >> "$RELATIONS"
            fi
        fi
    done < "$TMPDIR/calls.tmp"
    
    # Extraer COPY (nombre del copybook)
    awk 'substr($0,7,1) != "*" && index($0," COPY ") {
        line = substr($0,8)
        # Buscar COPY seguido de nombre
        if (match(line, /COPY [A-Z0-9]+/)) {
            copy_part = substr(line, RSTART, RLENGTH)
            gsub(/COPY /, "", copy_part)
            # Quitar punto si existe
            gsub(/\..*/, "", copy_part)
            print copy_part
        }
    }' "$TMPFILE" | sort -u > "$TMPDIR/copies.tmp"
    
    # Procesar COPYs encontrados
    while read _copybook; do
        if [ -n "$_copybook" ]; then
            if ! grep -q "^${_copybook}$" "$FOUND_COPY" 2>/dev/null; then
                echo "$_copybook" >> "$FOUND_COPY"
            fi
            # Guardar relacion: padre|hijo|tipo
            echo "${_pgm}|${_copybook}|CPY" >> "$RELATIONS"
        fi
    done < "$TMPDIR/copies.tmp"
    
    rm -f "$TMPFILE" "$TMPDIR/calls.tmp" "$TMPDIR/copies.tmp"
}

# Funcion para analizar un copybook
analyze_copybook() {
    _cpy=$1
    _parent=$2
    
    # Verificar si ya fue procesado
    if is_processed "$PROCESSED_COPY" "$_cpy"; then
        return
    fi
    mark_processed "$PROCESSED_COPY" "$_cpy"
    
    # Verificar si existe
    if ! member_exists "$COPYLIB" "$_cpy"; then
        return
    fi
    
    echo "Analizando copybook: $_cpy"
    
    # Copiar a temporal
    cp "//'${COPYLIB}(${_cpy})'" "$TMPFILE"
    
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
    
    # Procesar COPYs anidados
    while read _nested; do
        if [ -n "$_nested" ]; then
            if ! grep -q "^${_nested}$" "$FOUND_COPY" 2>/dev/null; then
                echo "$_nested" >> "$FOUND_COPY"
            fi
            # Guardar relacion: padre|hijo|tipo
            echo "${_cpy}|${_nested}|CPY" >> "$RELATIONS"
        fi
    done < "$TMPDIR/nested.tmp"
    
    rm -f "$TMPFILE" "$TMPDIR/nested.tmp"
}

# === INICIO DEL ANALISIS ===

# Paso 1: Analizar programa principal
analyze_program "$PGM" ""

# Paso 2: Loop hasta que no haya nuevos
ITERATION=0
MAX_ITER=50
while [ $ITERATION -lt $MAX_ITER ]; do
    ITERATION=`expr $ITERATION + 1`
    HAY_NUEVOS=0
    
    # Procesar programas encontrados
    if [ -f "$FOUND_PGMS" ]; then
        cat "$FOUND_PGMS" > "$TMPDIR/pgms_to_check.tmp"
        while read _p; do
            if [ -n "$_p" ]; then
                if ! is_processed "$PROCESSED_PGMS" "$_p"; then
                    HAY_NUEVOS=1
                    analyze_program "$_p" ""
                fi
            fi
        done < "$TMPDIR/pgms_to_check.tmp"
    fi
    
    # Procesar copybooks encontrados
    if [ -f "$FOUND_COPY" ]; then
        cat "$FOUND_COPY" > "$TMPDIR/copy_to_check.tmp"
        while read _c; do
            if [ -n "$_c" ]; then
                if ! is_processed "$PROCESSED_COPY" "$_c"; then
                    HAY_NUEVOS=1
                    analyze_copybook "$_c" ""
                fi
            fi
        done < "$TMPDIR/copy_to_check.tmp"
    fi
    
    # Si no hay nuevos, salir
    if [ $HAY_NUEVOS -eq 0 ]; then
        break
    fi
done

rm -f "$TMPDIR/pgms_to_check.tmp" "$TMPDIR/copy_to_check.tmp"

# === ARBOL DE DEPENDENCIAS ===
# Generar arbol y guardarlo en archivo
echo "" >> "$TREE"
echo "=========================================" >> "$TREE"
echo "       ARBOL DE DEPENDENCIAS" >> "$TREE"
echo "=========================================" >> "$TREE"
echo "[PGM] $PGM" >> "$TREE"

# Crear archivo para tracking de nodos visitados (evitar ciclos)
VISITED="$TMPDIR/visited_tree.txt"
rm -f "$VISITED"
touch "$VISITED"
echo "$PGM" >> "$VISITED"

# Imprimir arbol iterativo (evita problemas de recursion en z/OS shell)
print_tree_iterative "$PGM" "$VISITED" >> "$TREE"

rm -f "$VISITED"
echo "=========================================" >> "$TREE"

# Mostrar arbol en pantalla tambien
cat "$TREE"
echo ""
echo "Arbol guardado en: $TREE"

# === RESUMEN ===
echo ""
echo "=== RESUMEN ==="
echo ""
echo "Programas COBOL encontrados (CALL):"
if [ -s "$FOUND_PGMS" ]; then
    sort -u "$FOUND_PGMS"
else
    echo "  (ninguno)"
fi

echo ""
echo "Copybooks encontrados (COPY):"
if [ -s "$FOUND_COPY" ]; then
    sort -u "$FOUND_COPY"
else
    echo "  (ninguno)"
fi

echo ""
TOTAL_PGM=0
TOTAL_CPY=0
if [ -s "$FOUND_PGMS" ]; then
    TOTAL_PGM=`sort -u "$FOUND_PGMS" | wc -l`
fi
if [ -s "$FOUND_COPY" ]; then
    TOTAL_CPY=`sort -u "$FOUND_COPY" | wc -l`
fi
echo "Total programas COBOL: $TOTAL_PGM"
echo "Total copybooks: $TOTAL_CPY"

# === COPIAR ARCHIVOS ===
echo ""
echo "=== Copiando archivos ==="

# Crear directorios si no existen
if [ ! -d "$COBOL_DIR" ]; then
    mkdir -p "$COBOL_DIR"
    echo "Directorio creado: $COBOL_DIR"
fi
if [ ! -d "$COPY_DIR" ]; then
    mkdir -p "$COPY_DIR"
    echo "Directorio creado: $COPY_DIR"
fi

# Copiar programa principal
echo ""
echo "Copiando programas COBOL..."
cp "//'${PGMLIB}(${PGM})'" "$COBOL_DIR/${PGM}.cbl" 2>/dev/null
if [ $? -eq 0 ]; then
    echo "  Copiado: $PGM -> $COBOL_DIR/${PGM}.cbl"
fi

# Copiar programas COBOL encontrados
if [ -s "$FOUND_PGMS" ]; then
    sort -u "$FOUND_PGMS" | while read _pgm; do
        if [ -n "$_pgm" ]; then
            cp "//'${PGMLIB}(${_pgm})'" "$COBOL_DIR/${_pgm}.cbl" 2>/dev/null
            if [ $? -eq 0 ]; then
                echo "  Copiado: $_pgm -> $COBOL_DIR/${_pgm}.cbl"
            else
                echo "  ERROR copiando: $_pgm"
            fi
        fi
    done
fi

# Copiar copybooks
echo ""
echo "Copiando copybooks..."
if [ -s "$FOUND_COPY" ]; then
    sort -u "$FOUND_COPY" | while read _cpy; do
        if [ -n "$_cpy" ]; then
            cp "//'${COPYLIB}(${_cpy})'" "$COPY_DIR/${_cpy}.cpy" 2>/dev/null
            if [ $? -eq 0 ]; then
                echo "  Copiado: $_cpy -> $COPY_DIR/${_cpy}.cpy"
            else
                echo "  ERROR copiando: $_cpy"
            fi
        fi
    done
fi

echo ""
echo "========================================="
echo "Programas COBOL en: $COBOL_DIR"
echo "Copybooks en: $COPY_DIR"
echo "========================================="

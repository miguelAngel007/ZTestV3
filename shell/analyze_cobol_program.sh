PGMLIB="E.LIB.PROGRAM"
PGM=IM31
CALLTARGETLIB="E.LIB.PROGRAM"

# Copiar el programa del PDS a temporal
TMPFILE="/u/idz/B45617/tmp/${PGM}.tmp"
cp "//'${PGMLIB}(${PGM})'" "$TMPFILE"

# Usar awk para filtrar: no comentarios y que contengan COPY o CALL
awk 'substr($0,7,1) != "*" && (index($0," COPY ") || index($0," CALL ")) { print substr($0,8) }' "$TMPFILE"

# Limpiar temporal
rm -f "$TMPFILE"


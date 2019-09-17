#shell script to see if there are tags which are not declared in root.lexc or if tags are misspelled

echo 'Are there tags not declared in root.lexc or misspelled?'
cat src/morphology/compounding.lexc \
    src/morphology/affixes/*lexc \
    src/morphology/stems/*lexc  ../../giella-shared/smi/src/morphology/stems/*lexc | \
    cut -d '!' -f1 |grep ' ;' | cut -d ':' -f1 |tr -s ' ' |sed 's/^ //' | cut -d ' ' -f1 |sed 's/+/¢+/g' |sed 's/@/¢@/g'|tr '¢' '\n' | tr '#"' '\n'| egrep '(\+|@)' |sort -u | egrep -v '^(\+|\+%|\+\/\-|\+Cmp\-|\+Cmp%\-|\@0|\@%)$' > lexctags

cat src/morphology/root.lexc |cut -d '!' -f1 |cut -d ':' -f1 |sed 's/+/¢+/g'|sed 's/@/¢@/g' |tr '¢' '\n' | egrep '(\+|@)' |tr -d ' ' | tr -d '\t'|sort -u > roottags

echo 'Have a look at these:'

comm -23 lexctags roottags 

echo 'checked'
echo '\n'
echo 'Note for the Finnish output:'
echo 'The forms'
echo '+3, +Bickenbach, +Kennedy'
echo 'are actually intended strings and should not be declared in root.lexc.'

rm lexctags roottags

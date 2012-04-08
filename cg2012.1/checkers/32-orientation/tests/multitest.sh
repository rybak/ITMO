argc=$#

if [ $argc -eq 0 ]
then
    echo "Please specify the path to executable"
    exit
fi

solution=$1

for testName in *.test
do
    echo "Processing $testName"
    $solution < $testName > "$testName.ans"
    if [ $argc -eq 2 ]
    then
        $2 $testName "$testName.ans" 
        if [ $? -eq 0 ]
        then
            echo "OK"
        else
            echo "INCORRECT"
        fi
    fi    
    echo 
    #sleep 1
done

rm *.test.ans

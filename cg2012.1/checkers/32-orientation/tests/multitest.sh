if [ "$#" -eq 0 ]
then
    echo "Please specify the path to executable"
    exit
fi

solution=$1

for testName in *.test
do
    echo "Processing $testName"
    $solution < $testName
    echo 
done

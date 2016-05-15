server="localhost:8000"

seq 0 128 | xargs -I{} curl -s --data "input={}" $server/jobs

while true
do
    jobId=$(curl -s $server/requestJob)
    if [[ -n "$jobId" ]]; then
        printf "Processing @$jobId .. "
        input=$(curl -s $server/jobs/$jobId)
        sleep 1
        result=$(curl -is --data "output=$input" $server/results/$jobId)
        printf "OK\n"
    else
        sleep 1
    fi 
done

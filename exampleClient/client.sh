server="localhost:8000"

seq 0 16 | xargs -I{} curl -s --data "input={}" $server/jobs

while true
do
    jobId=$(curl -s $server/requestJob)
    input=$(curl -s $server/jobs/$jobId)
    printf "Processing @$jobId .. "
    sleep 1
    printf "OK\n"
done

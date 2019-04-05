for file in test/*.Cal; do 
    echo -e "\033[0;32mTesting $file \033[0m"; 
    ./source <"$file";
    echo -e "\r"; 
done;

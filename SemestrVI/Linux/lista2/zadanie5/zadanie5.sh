function print_entropy {
	while true
	do
		printf "Available entropy: %s/%s" \
		   "$(</proc/sys/kernel/random/entropy_avail)" \
		   "$(</proc/sys/kernel/random/poolsize)"
	    sleep 1s
	    printf $'\r'
	done
}

print_entropy &
read -n 1 -r -s
kill %1
printf '\n'

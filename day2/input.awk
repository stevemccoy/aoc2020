BEGIN { FS = "[- :]+"; count = 0 }
{
    re = $3 "{" $1 "," $2 "}"
    if (match($4, re) != 0)
	print $0
}

BEGIN { FS = "[- :]+"; count = 0 }
{
	print "input([" $1 "," $2 ",'" $3 "','" $4 "']).";
}

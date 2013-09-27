lines = readlines.map { |line|
	line.match(/
		^
		(\d{4}\/\d{2}\/\d{2})\s+ # Date
		(\d{2}:\d{2}:\d{2})\s+ # Time
		(\[\w+\])\s+ # Tag
		(\d+)\# # Error code no 1?
		(\d+):\s+ # Error code no 2?
		(.+?),\s+ # Error message
		# Additional fields
			(?:
				(\w+):\s+ # Field name
				(.+?) # Field value
				(?:,\s*|$)
			)*
		\s+
		$
	/x)
}.select{|i| i != nil}.map { |line|
	line = line.to_a
	_, date, time, tag, errno1, errno2, message = line
	[date, time, tag, errno1, errno2, message]
	data = {
		:date => date,
		:time => time,
		:tag => tag,
		:errno1 => errno1,
		:errno2 => errno2,
		:message => message
	}
	rest = line.to_a[data.size+1..line.size]
	
}

# puts lines.reduce({}) { |hits, line|
# 	date = line[1]
# 	hits[date] =  hits[date].to_i + 1

# 	hits
# }

puts lines
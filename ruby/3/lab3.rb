lines = readlines.map { |line|
	# Parse each line as regexp
	line.match(/
		^
		((\d{4})\/(\d{2})\/(\d{2}))\s+ # Date
		((\d{2}):(\d{2}):(\d{2}))\s+ # Time
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
	# Convert metch result into a hash
	line = line.to_a
	_, date, year, month, day, time, hour, min, sec, tag, errno1, errno2, message, *rest = line
	
	data = {
		:date => date,
		:year => year,
		:month => month,
		:day => day,
		:time => time,
		:hour => hour,
		:min => min,
		:sec => sec,
		:tag => tag,
		:errno1 => errno1,
		:errno2 => errno2,
		:message => message
	}
	data.merge Hash[rest.each_slice(2).to_a]
}
puts lines[1..3], "\n"

# Hits per hour
puts lines.reduce({}) { |hits, record|
	date = record[:date] + " " + record[:hour]
	hits[date] =  hits[date].to_i + 1

	hits
}, "\n"

# Error types
puts lines.reduce({}) { |hits, record|
	errno = record[:errno1] + " " + record[:errno2]
	hits[errno] =  hits[errno].to_i + 1

	hits
}


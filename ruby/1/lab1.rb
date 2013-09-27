def words(n, alpha) 
	(1..n).reduce([""]) { |fullList, _|
		fullList.reduce([]) { |lst, word|
			lst + alpha.select {|symbol| symbol != word[-1]} .map {|symbol| word + symbol}
		}
	}
end

print (words 3, ['a', 'b', 'c']), "\n"


-- my2c.ex impliments a new language to be used with C/C++ compilers

without type_check

include std/types.e


global integer fn, header, in

include my2c.e

--constant outname = "*.jjc"

constant cmdln = command_line()

sequence filenames = cmdln[3..$]

sequence parsed, cpp_code
parsed = {}
cpp_code = {}

sequence nextline = {}

procedure set_nextline()
	object line
	if length(nextline) = 0 then
		line = gets(in)
		if atom(line) then
			nextline = {}
		else
			nextline = line
		end if
	end if
end procedure

sequence lineBuffer
lineBuffer = {}
function get_nextCh()
	integer ch
	while not length(lineBuffer) do
		set_nextline()
		if length(nextline) = 0 then
			return -1
		end if
-- 		if nextline[$] = '\n' then
-- 			nextline = nextline[1..$-1]
-- 		end if
		lineBuffer = nextline
		nextline = {}
	end while
	ch = lineBuffer[1]
	lineBuffer = lineBuffer[2..$]
	return ch
end function

-- 	Defined_Sets[CS_Alphabetic	] = {{'a', 'z'}, {'A', 'Z'}}
-- 	Defined_Sets[CS_Alphanumeric] = {{'0', '9'}, {'a', 'z'}, {'A', 'Z'}}
-- 	Defined_Sets[CS_Identifier]   = {{'0', '9'}, {'a', 'z'}, {'A', 'Z'}, {'_', '_'}}
-- 	Defined_Sets[CS_Uppercase 	] = {{'A', 'Z'}}
-- 	Defined_Sets[CS_Lowercase 	] = {{'a', 'z'}}
-- 	Defined_Sets[CS_Printable 	] = {{' ', '~'}}
-- 	Defined_Sets[CS_Displayable ] = {{' ', '~'}, "  ", "\t\t", "\n\n", "\r\r", {8,8}, {7,7} }
-- 	Defined_Sets[CS_Whitespace 	] = " \t\n\r" & 11 & 160
-- 	Defined_Sets[CS_Consonant 	] = "bcdfghjklmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ"
-- 	Defined_Sets[CS_Vowel 		] = "aeiouAEIOU"
-- 	Defined_Sets[CS_Hexadecimal ] = {{'0', '9'}, {'A', 'F'},{'a', 'f'}}
-- 	Defined_Sets[CS_Punctuation ] = {{' ', '/'}, {':', '?'}, {'[', '`'}, {'{', '~'}}
-- 	Defined_Sets[CS_Control 	] = {{0, 31}, {127, 127}}
-- 	Defined_Sets[CS_ASCII 		] = {{0, 127}}
-- 	Defined_Sets[CS_Digit 		] = {{'0', '9'}}
-- 	Defined_Sets[CS_Graphic 	] = {{'!', '~'}}
-- 	Defined_Sets[CS_Bytes	 	] = {{0, 255}}
-- 	Defined_Sets[CS_SpecWord 	] = "_"
-- 	Defined_Sets[CS_Boolean     ] = {TRUE,FALSE}

constant quotes = "\"" -- "\'"

sequence stored_whitespace

function get_nextword()
	sequence word
	integer ch, f
	word = {}
	
	stored_whitespace = ""
	ch = get_nextCh()
	-- store whitespace:
	while t_space(ch) do
		stored_whitespace = stored_whitespace & {ch}
		ch = get_nextCh()
	end while
	
	-- print "c/c++" code
	if ch = 'c' or ch = 'h' then
		f = get_nextCh()
		if t_space(f) then
			-- return rest of the line
			word = lineBuffer
			lineBuffer = {}
			if ch = 'c' then
				return {"c", word}
			else
				return {"h", word}
			end if
		else
			-- push f
			lineBuffer = {f} & lineBuffer
		end if
	end if
	
	-- process quotes:
	for i = 1 to length(quotes) do
		if ch = quotes[i] then
			word = {quotes[i]}
			f = -2
			while f = -2 do
				f = find(quotes[i], lineBuffer)
				if f > 1 then
					if lineBuffer[f-1] = '\\' then
						-- add everything up until this point to "word"
						word = word & lineBuffer[1..f]
						lineBuffer = lineBuffer[f+1..$]
						f = -2
					end if
				end if
			end while
			if f then
				word = word & lineBuffer[1..f]
				lineBuffer = lineBuffer[f+1..$]
			end if
			return {quotes[i], word}
		end if
	end for
	
	-- process identifier:
	while t_identifier(ch) do
		word = word & {ch}
		ch = get_nextCh()
	end while
	if length(word) then
		-- push ch
		lineBuffer = {ch} & lineBuffer
		return {"", word}
	end if
	
	-- process line comments:
	if ch = '/' then
		ch = get_nextCh()
		if ch = '/' then
			-- comment out the rest of the line
			word = lineBuffer
			lineBuffer = {}
			return {"//", word}
		else
			-- push ch
			lineBuffer = {ch} & lineBuffer
		end if
	end if
	
	while ch != ',' and ch != ';' do
		word = word & {ch}
		ch = get_nextCh()
		if ch = -1 then
			exit
		end if
	end while
	if length(word) then
		-- push ch
		lineBuffer = {ch} & lineBuffer
		return {"", word}
	end if
	
	--if t_punct(ch) then
	return {"", {ch}}
	--end if
	
end function

function get_next_statement()
	sequence ret, s
	integer ch
	
	s = get_nextword()
	if s[2][$] = -1 then
		return -1
	end if
	
	-- insert "c/c++" code
	if equal("c", s[1]) or equal("h", s[1])then
		return {stored_whitespace} & s
	end if
	
	-- comment:
	if equal("//", s[1]) then
		return {stored_whitespace} & s
	end if
	
	-- functions:
	if equal("", s[1]) then
		
		--done. Don't modify:
		
		ret = {stored_whitespace, s[2]}
		
		while 1 do
-- 			stored_whitespace = ""
-- 			ch = get_nextCh()
-- 			-- store whitespace:
-- 			while t_space(ch) do
-- 				stored_whitespace = stored_whitespace & {ch}
-- 				ch = get_nextCh()
-- 			end while
-- 			-- push ch
-- 			lineBuffer = {ch} & lineBuffer
			
			s = get_nextword()
			if s[2][$] = -1 then
				return -1
			end if
			
			if equal("", s[1]) then
				-- end statement:
				if equal(";", s[2]) then
					exit
				end if
			end if
			-- comment:
			if equal("//", s[1]) then
				-- push comment
				lineBuffer = s[1] & s[2]
				
				-- enables multiline print
				set_nextline()
				nextline = ret[1] & " " & nextline
				
				exit
			end if
-- 			-- quotes:
-- 			if find(s[1], quotes) then
-- 				s[2] = "\"" & s[2][2..$-1] & "\""
-- 			end if
			
			ret = ret & {stored_whitespace, s[2]}
			
		end while
		
		-- return value:
		return ret
		
	end if
	
	return -2
	
end function

procedure process_lines()
	sequence name, ws
	object line, tmp
	while 1 do
		line = get_next_statement()
		if atom(line) then
			return
		end if
		
		ws = line[1]
		line = line[2..$]
		
		if equal("h", line[1]) then
			line = sprintf("%s", {line[2]})
			line = ws & line
			print_header(line)
		else
		
		switch line[1] do
		case "//" then
			line = sprintf("//%s", {line[2]})
		case "c" then
			line = sprintf("%s", {line[2]})
		case "print" then
			tmp = line[2..$] -- ws
			line = "std::cout <<"
			while length(tmp) do
				line = line & tmp[1] -- ws
				tmp = tmp[2..$] -- not ws
				if not equal(",", tmp[1]) then -- not ws
					line = line & tmp[1] & " <<" -- not ws
				end if
				tmp = tmp[2..$] -- ws
			end while
			line = line & " std::endl;"
			--line = sprintf("\tputs( \"%s\\n\" );\n", {tmp})
			--tmp = {}
		case else
			name = line[1]
			tmp = {}
			if length(line) > 1 then
				line = line[2..$]
				while length(line) do
					tmp = tmp & line[1]
					line = line[2..$]
				end while
			end if
			line = sprintf("%s(%s );", {name, tmp})
			tmp = {}
		end switch
		
-- 		if length(lineBuffer) then
-- 			line[$] = ' '
-- 		end if
		
		line = ws & line
		puts(fn, line)
			
		end if
		
	end while
	
end procedure



for i = 1 to length(filenames) do
	in = open(filenames[i], "r")
	if in < 0 then
		printf(2, "Error: Couldn't open file: \"%s\"\n", {filenames[i]})
		abort(1)
	end if
	
	header = open(sprintf("%s.hpp", {filenames[i]}), "w")
	if in < 0 then
		printf(2, "Error: Couldn't open file: \"%s.hpp\"\n", {filenames[i]})
		abort(1)
	end if
	fn = open(sprintf("%s.cpp", {filenames[i]}), "w")
	if in < 0 then
		printf(2, "Error: Couldn't open file: \"%s.cpp\"\n", {filenames[i]})
		abort(1)
	end if
	
	
	set_nextline()
	
	while length(nextline) >= 2 and nextline[1] = '/' and nextline[2] = '/' do
		puts(header, nextline)
		puts(fn, nextline)
		nextline = {}
		set_nextline()
	end while
	
	print_top_header()
	
	print_top()
	
	puts(fn, sprintf("#include \"%s.hpp\"\n\n", {filenames[i]}))
	print_main()
	
	process_lines()
	
	print_return("0")
	print_end()
	
	puts(header, "\n// End of file.\n")
	
	close(in)
	close(header)
	close(fn)
end for

sequence myname = cmdln[2]
integer f

f = length(myname)
while f > 0 do
	if myname[f] = '\\' or myname[f] = '/' then
		exit
	end if
	f -= 1
end while

myname = myname[1+f..$]

printf(1, "Program \"%s\" done.\n", {myname})

-- end of file

/*

         CONTENTS - (Use <ENTER> g to access required sections)

 define next_word(i,string,linkers) -> i -> j -> word;
 define prev_word(i,string,linkers) -> j -> i -> word;
 define ismixedcase(string);
 define isuppercase(string);
 define insert_html_links(system,line);
 define isvedgraphic(char);
 define insert_html_tags(line);
 define pr_line(line);
 define insert_index_links(system,type,line);
 define insert_man_links(line,keyword);
 define sysmanfile(name,sect);
 define mantohtml(sect,name);
 define indextohtml(system,type,name);
 define is_index_line(line) -> type -> index;
 define old_index_line(line,contents) -> index;
 define vedtohtml(system,type,name);
 define cgi_to_vedhtml();

*/

;;;if using popc link in the subsytems instead of "auto" loading them
#_IF DEF POPC_COMPILING
    uses lisp_subsystem
    uses ml_subsystem
    uses prolog_subsystem
#_ENDIF


compile_mode :pop11 +strict;

/*
default base_url may be overriden by the environment variable $usepop_base_url
*/
vars base_url = 'http://www.poplog.cs.reading.ac.uk/cgi-bin/poplog/';


define next_word(i,string,linkers) -> i -> j -> word;
lvars i, j string len word, linkers;
    length(string) -> len;
    for i from i to len do
        quitif(isalphacode(string(i)));
    endfor;
    if i > len then
        false -> word;
        return();
    endif;
    for j from i+1 to len do
        quitunless(
            isalphacode(string(j))
            or isnumbercode(string(j))
            or locchar(string(j),1,linkers)
        );
    endfor;
    ;;;substring(i,j-i,string) -> word;
    until (j-1->>j) < i or isalphacode(string(j)) or isnumbercode(string(j)) do;    enduntil;
    if j < i then
        false -> word;
    else
        ;;;j + 1 -> j;
        substring(i,j-i+1,string) -> word;
    endif;
enddefine;

define prev_word(i,string,linkers) -> j -> i -> word;
lvars i, j string ;
    for i from i by -1 to 1 do
        quitif(isalphacode(string(i)) or isnumbercode(string(i)));
    endfor;
    if i < 1 then false -> word; return(); endif;
    for j from i-1 by -1 to 1 do
        quitunless(
            isalphacode(string(j))
            or isnumbercode(string(j))
            or locchar(string(j),1,linkers)
        );
    endfor;

    until (j+1->>j) > i or isalphacode(string(j)) do; enduntil;
    if j > i then
        false -> word;
    else
        substring(j,i-j+1,string) -> word;
    endif;
enddefine;

define ismixedcase(string);
lvars i string, upper=false, lower=false;
    for i from 1 to length(string) do
        if isuppercode(string(i))
        then true -> upper;
        else true -> lower;
        endif;
    endfor;
    return(upper and lower);
enddefine;

define isuppercase(string);
lvars i string;
    for i from 1 to length(string) do
        unless isuppercode(string(i)) or string(i) = `_`
        then return(false);
        endunless;
    endfor;
    return(true);
enddefine;

define insert_html_links(system,line);
    lvars i j k l line type name system patern prev_type = false;
    lconstant types = ['help' 'ref' 'teach' 'doc' 'man' 'unix' 'ploghelp' ],
        end_anchor = consword('</A>');

    1 -> k;
    [%

        while (locchar(`*`,k,line) ->> i) do
            prev_word(i,line,nullstring) -> j; -> ; -> type;
            if type and member(uppertolower(type) ->>type,types) then
                type -> prev_type;
            elseif prev_type then
                prev_type -> type;
                i -> j;
            else
                false -> type;
            endif;
            if type
            and (next_word(i,line,'_.') -> ; -> l; ->> name)
            then
                if name.isuppercase
                then uppertolower(name) -> name;
                endif;
                subdstring(k,j-k,line);
                if l < length(line) and line(l+1) = `/`
                and (next_word(l+1,line,'_.') ->; -> i; ->> patern)
                then
                    consword('<A HREF="'><system><'/'><type><'/'><name><'#'><patern><'">');
                    i -> l;
                else
                    consword('<A HREF="'><system><'/'><type><'/'><name><'">');
                endif;
                substring(j,l-j+1,line);
                end_anchor;
                l + 1 -> k;
            else
                subdstring(k,i-k+1,line);
                i + 1 -> k;
            endif;
        endwhile;
        if k = 1 then
            line;
        elseif k < length(line) then
            subdstring(k,length(line)-k+1,line);
        endif;
    %]
enddefine;

define isvedgraphic(char);
lvars char;
    char && 16:FF -> char;
    char > 16:80 and char < 16:A0;
enddefine;

include vedscreendefs;

/*
   lynx uses underscore for bold and alt characters
   However, ved assumes that the normal, bold and alt spaces are the same
   so does not end the mode until the next non space characters.
   The routine strips any trailing spaces of the previous string.
*/
	
define lynx_end_mode_hack(string,end_mode);
lvars string end_mode i len;
	unless string.isstring then
        if not(string.isword) or string(1) /== `<` or string(2) = end_mode(3) then
			;;;don't do anything;
			string; end_mode;	
		else
			lynx_end_mode_hack(end_mode);string;
	    	return();
		endif;
	endunless;
	length(string) -> len;
	;;;check for trailing spaces
	if (skipchar_back(` `,len,string)->>i) = len then
		;;;no spaces so don't do anything
		string; end_mode;
	else
		if i then 	;;;insert end mode to before trailing spaces
			substring(1,i,string); end_mode; substring(i+1,len-i,string);
		else		;;;string is all spaces so swap sting and end_mode
			end_mode; string;
		endif;
	endif;
enddefine;

define insert_html_tags(line);
lvars i j k segment line mode change;
lconstant ATRIBMASK = VEDCMODE_UNDERLINE || VEDCMODE_ALTFONT || VEDCMODE_BOLD || VEDCMODE_BLINK,
    start_alt   = consword('<I>'),    end_alt   = consword('</I>'),
    start_bold  = consword('<B>'),    end_bold  = consword('</B>'),
    start_under = consword('<U>'),    end_under = consword('</U>'),
    start_blink = consword('<BLINK>'),end_blink = consword('</BLINK>'),
    spaces = ' \Sf\Ss\St\Sp\Sn\^]',
    special_chars = '<>&"',
    special_entities = {%
        consword('&lt;'),  consword('&gt;'),  consword('&amp;'), consword('&quot;'),
    %},
    graphic_chars = '\G.\G#\Go\G+\G|\G-\Gle\Gre\Gte\Gbe\Glt\Grt\Gtt\Gbt\Gtl\Gtr\Gbl\Gbr',
    graphic_entities = {%
        consword('&#176'),	;;;\G.
		consword('&#164'),  ;;;\G#
		consword('&#176'),  ;;;\Go
        consword('&#43'),   ;;;\G+
		consword('&#124'),  ;;;\G|
		consword('&#45'),   ;;;\G-
    	consword('&#45'),   ;;;\Gle
		consword('&#45'),   ;;;\Gre
		consword('&#124'),  ;;;\Gte
		consword('&#124'),  ;;;\Gbe
    	consword('&#124'),  ;;;\Glt
		consword('&#124'),  ;;;\Grt
		consword('&#45'),   ;;;\Gtt
		consword('&#43'),   ;;;\Gbt
    	consword('&#43'),   ;;;\Gtl
		consword('&#43'),   ;;;\Gtr
		consword('&#43'),   ;;;\Gbl
		consword('&#43'),   ;;;\Gbr
     %},
;

    [%
        false -> mode;
        for segment in line do
            1 -> i;
            if segment.isdstring then
                unless mode then
                    segment(i) && ATRIBMASK -> mode;
                    if mode && VEDCMODE_ALTFONT = VEDCMODE_ALTFONT
                    then start_alt;
                    endif;
                    if mode && VEDCMODE_BOLD = VEDCMODE_BOLD
                    then start_bold;
                    endif;
                    if mode && VEDCMODE_UNDERLINE = VEDCMODE_UNDERLINE
                    then start_under;
                    endif;
                    if mode && VEDCMODE_BLINK = VEDCMODE_BLINK
                    then start_blink;
                    endif;
                endunless;
                for j from 1 to length(segment) do
                    if locchar(segment(j),1,spaces) then
                        ` ` -> segment(j);
                        nextloop;
                    endif;
                    unless ((segment(j) && ATRIBMASK) ||/& mode ->> change) = 0 then
                        substring(i,j-i,segment);
                        if change && VEDCMODE_ALTFONT = VEDCMODE_ALTFONT then
                            if mode && VEDCMODE_ALTFONT = VEDCMODE_ALTFONT
                            then
								;;;end_alt;
								lynx_end_mode_hack(end_alt);
                            else start_alt;
                            endif;
                        endif;
                        if change && VEDCMODE_BOLD = VEDCMODE_BOLD then
                            if mode && VEDCMODE_BOLD = VEDCMODE_BOLD
                            then
								;;;end_bold;
								lynx_end_mode_hack(end_bold);
                            else start_bold;
                            endif;
                        endif;
                        if change && VEDCMODE_UNDERLINE = VEDCMODE_UNDERLINE then
                            if mode && VEDCMODE_UNDERLINE = VEDCMODE_UNDERLINE
                            then ;;;end_under;
								lynx_end_mode_hack(end_under);
                            else start_under;
                            endif;
                        endif;
                        if change && VEDCMODE_BLINK = VEDCMODE_BLINK then
                            if mode && VEDCMODE_BLINK = VEDCMODE_BLINK
                            then ;;;end_blink;
								lynx_end_mode_hack(end_blink);
                            else start_blink;
                            endif;
                        endif;
                        j -> i;
                        segment(j) && ATRIBMASK -> mode;
                    endunless;
                    if (segment(j).isvedgraphic) then
                        if (locchar(segment(j),1,graphic_chars) ->>k) then
                            substring(i,j-i,segment);
                            graphic_entities(k);
                            j + 1 -> i;
                        endif;
                    elseif (locchar(segment(j),1,special_chars) ->>k) then
                        substring(i,j-i,segment);
                        special_entities(k);
                        j + 1 -> i;
                    endif;
                endfor;
            elseif segment.isstring then
                1 -> i;
                for j from 1 to length(segment) do
                    if (segment(j).isvedgraphic) then
                        if (locchar(segment(j),1,graphic_chars) ->>k) then
                            substring(i,j-i,segment);
                            graphic_entities(k);
                            j + 1 -> i;
                    	elseif locchar(segment(j),1,spaces) then
                        	` ` -> segment(j);
						endif;
                    elseif (locchar(segment(j),1,special_chars) ->>k) then
                        substring(i,j-i,segment);
                        special_entities(k);
                        j + 1 -> i;
                    endif;
                endfor;
            endif;
            if i = 1 then
                segment;
            elseif i <= length(segment) then
                substring(i,length(segment)-i+1,segment);
            endif;
        endfor;
        if mode then
            if mode && VEDCMODE_BOLD = VEDCMODE_BOLD
            then end_bold;
            endif;
            if mode && VEDCMODE_ALTFONT = VEDCMODE_ALTFONT
            then end_alt;
            endif;
            if mode && VEDCMODE_UNDERLINE = VEDCMODE_UNDERLINE
            then end_under;
            endif;
            if mode && VEDCMODE_BLINK = VEDCMODE_BLINK
            then end_blink;
            endif;
        endif;
    %]

enddefine;

define pr_line(line);
lvars seg line;
    for seg in line do
        pr(seg);
    endfor;
    nl(1);
enddefine;

define insert_index_links(system,type,line);
    lvars i j k line type system name;
    lconstant end_anchor = consword('</A>');
    1 -> k;
    [%
        while (next_word(k,line,'_.') -> i -> j ->> name) do
            substring(k,i-k,line);
            if name.isuppercase
            then uppertolower(name) -> name;
            endif;
            consword('<A HREF="'><system><'/'><type><'/'><name><'">');
            name;
            end_anchor;
            j + 1 -> k;
        endwhile;
        if k = 1 then
               line;
        else
            substring(k,length(line)-k+1,line);
        endif;
    %];
enddefine;

define insert_man_links(line,keyword);
    lvars i j k l line type name system keyword  prev_type = false;
    lconstant end_anchor = consword('</A>');
    1 -> k;
    [%
        while (locchar(`(`,k,line) ->> j) do
            if j > 1 and j < length(line)
            and (
				isalphacode(line(j-1)) or isnumbercode(line(j-1))
				or line(j-1)=`+`				 ;;;ie NIS+
				or (keyword and line(j-1) = ` `) ;;;man -k puts a space before "("
			)
            and isnumbercode(line(j+1))
            and (prev_word(j,line,'._-+') -> i; -> ; ->> name)
            and (locchar(`)`,j,line) ->> l)
            then
                subdstring(k,i-k,line);
                subdstring(j+1,l-j-1,line) -> type;
;;;Hack for Solaris 3X files
if hasstartstring(type,'3X') then '3' -> type; endif;
                consword('<A HREF="man/'><type><'/'><substring(i,j-i,line)><'">');
                subdstring(i,l-i+1,line);
                l + 1 -> k;
                end_anchor;
            else
                subdstring(k,j-k+1,line);
                j+1 -> k;
            endif;
        endwhile;
        if k = 1 then
            line;
        elseif k < length(line) then
            subdstring(k,length(line)-k+1,line);
        endif;
    %]
enddefine;

define sysmanfile(name,sect,path);
    lvars c name sect command ;
  
    if length(name) = 0 then return(false); endif;
    if strmember(`<`,name) then return(false); endif;
    if strmember(`|`,name) then return(false); endif;
    if strmember(`&`,name) then return(false); endif;
    if strmember(`>`,name) then return(false); endif;
    /* if string contains a ";" then only use the upto that charactor 
    if (strmember(`;`,name) ->> c) then
        substring(1,c,name) -> name;
    endif;
*/
    if strmember(`/`, name) then ;;;or strmember(`.`, name) then
        sysfileok(name) -> name;
        if readable(name) then
           'nroff -man ' <> name
        else
           false;
        endif;
    else
        ;;;only take the first token from name
        name.stringin.incharitem() >< nullstring-> name;
        /* convert all upper case names to lower (except X !) */
        if name /= 'X'
        and (true,  for c in_string name do
                    if islowercode(c) then ->, false, quitloop endif
                endfor)
        then
             uppertolower(name) -> name
        endif;

        #_IF DEF BERKELEY
        'man ' <> name <> ' | cat -s'
        #_ELSE

        'man ' ;
        unless  sect = nullstring then <> '-s ' <> sect <> ' '; endunless;
		if path then <> '-M ' <> path <> ' ' ; endif;
		<> name;

        #_ENDIF
    endif -> command;

    if command then
      pipein('/bin/sh', ['sh' '-ce' ^command], false);
    else
      false;
    endif

enddefine;

define mantohtml(sect,name, keyword);
    lvars sect name file rep list line keyword i path = false;
    dlocal poplinewidth = false; ;;;stop line breaks

    if (locchar(`:`,1,name) ->> i) then
        substring(i+1,length(name)-i,name) -> path;
        ;;;path.stringin.incharitem<>nonop ><(%nullstring%) . pdtolist-> path;
        insert_html_tags([^path]).hd >< nullstring -> path;
        substring(1,i-1,name) -> name;
    endif;

    ;;;name.stringin.incharitem<>nonop ><(%nullstring%).pdtolist-> name;
    insert_html_tags([^name]).hd >< nullstring -> name;
    if keyword then '-k '><name -> name; endif;


    pr('<HTML>\n<HEAD>\n<TITLE>\n');pr('MAN '); pr(name);
    unless sect = nullstring then
        pr('(');pr(sect);npr(')');
    else
        nl(1);
    endunless;
    pr('</TITLE>\n');
    pr('<BASE HREF="');pr(base_url);pr('">\n');
    pr('</HEAD>\n<BODY>\n');

    sysmanfile(name,sect,path) -> file;

    if file then 
        vedfile_line_repeater(file,`\[ib]`) -> rep;

	if keyword then
    	until (rep() ->> line) = termin do
        	insert_man_links(line,keyword) -> line;
        	pr('<p>\n');pr_line(line);
    	enduntil;
	else
    	pr('<PRE width=72>\n');
    	until (rep() ->> line) = termin do
        	insert_man_links(line,keyword) -> line;
        	insert_html_tags(line) -> line;
        	pr_line(line);
    	enduntil;
    	pr('</PRE>\n');
  	endif;
    else
      pr('<P>Man page '><name><' not found in section '><sect><'\n');
    endif;

    pr('</BODY>\n</HTML>\n');
enddefine;

uses ved_g;
define is_index_line(line) -> type -> index;
lvars line type index,i;
    if isstartstring(' -- ',line) then
        false -> type;
        if(skipchar(` `,5,line) ->> i) then
		substring(i,length(line)-i+1,line) -> index;
	else
	        substring(5,length(line)-4,line) -> index;
	endif;
    else
        if ($-ved$-is_new_indexline(line) ->> index) then
           true->type;
        else
            false -> type;
        endif;
    endif;
enddefine;

define old_index_line(line,contents) -> index;
lvars line index contents;
    for index in contents do
        if issubstring(index,line) then
            return();
        endif;
    endfor;
    false -> index;
enddefine;

define insert_contents_links(line,new_index,contents_list,url,system) -> line -> new_index -> contents_list;
    lvars i index new_index line contents_list,system,url;
    lconstant end_anchor = consword('</A>');

    if (is_index_line(line) -> i ->> index) then
        i -> new_index;
        if contents_list == [] or not(member(index,contents_list)) then
            contents_list nc_<> [ ^index ] -> contents_list;
        endif;
        next_word(1,line,nullstring) ->i; ->; -> ;
        [%
            subdstring(1,i-1,line);
            consword('<A HREF="'><url><'#'><index><'">');
            ;;;consword('<A HREF="#'><index><'">');
            substring(i,length(line)-i+1,line);
            end_anchor;
        %] -> line
    elseif contents_list /== [] then
        if new_index and member(line,contents_list) then
            next_word(1,line,nullstring) ->; ->; -> index;
            [%
                consword('<A NAME="'><index><'">');
                end_anchor;
                consword('<A NAME="'><line><'">');
                line;
                end_anchor;
            %] -> line;
            delete(line,contents_list) -> contents_list;
        elseif (old_index_line(line,contents_list) ->> index) then
            delete(index,contents_list) -> contents_list;
            [%
                consword('<A NAME="'><index><'">');
                end_anchor;
                next_word(1,index,nullstring) ->; ->; -> index;
                consword('<A NAME="'><index><'">');
                line;
                end_anchor;
            %] -> line;
        else
            insert_html_links(system,line) -> line;
        endif;
    else
        insert_html_links(system,line) -> line;
    endif;
enddefine;

define indextohtml(system,type,name);
    lvars type name file rep list line c s offset = 1, url;
    dlocal poplinewidth = false; ;;;stop line breaks
    lvars new_index = false, contents_list = [], contents = false;
    lconstant end_anchor = consword('</A>');

    if system = "pop11" then
        switchon type =
        case 'help'  then vedhelplist  -> list;
        case 'ref'   then vedreflist   -> list;
        case 'teach' then vedteachlist -> list;
        case 'doc'   then veddoclist   -> list;
        case 'ploghelp'  then subsystem_searchlist("vedhelpname","prolog"->>system)  -> list;
        else
            npr('Invalid Type "'><type><'" expected help, ref, teach or doc');
            return();
        endswitchon;
    else
        switchon type =
        case 'help'  then "vedhelpname"  -> list;
        case 'ref'   then "vedrefname"   -> list;
        case 'teach' then "vedteachname" -> list;
        case 'doc'   then "veddocname"   -> list;
        case 'ploghelp'  then "vedhelpname"  -> list;
        else
            npr('Invalid Type "'><type><'" expected help, ref, teach or doc');
            return();
        endswitchon;
        subsystem_searchlist(list,system) -> list;
    endif;

    syssearchpath(list,name) -> file;
    unless file then syssearchpath(list,lowertoupper(name)) -> file; endunless;
    unless file then syssearchpath(list,uppertolower(name)) -> file; endunless;

    pr('<HTML>\n<HEAD>\n<TITLE>\n');spr(system);spr(type);npr(name);
    pr('</TITLE>\n');
    pr('<BASE HREF="');pr(base_url);pr('">\n');
    pr('</HEAD>\n<BODY>\n');

    if file then
        if file.islist then hd(file) -> file ; endif;

        vedfile_line_repeater(file,false) -> rep;

        pr('<PRE width=72>\n');

	system >< '/' >< type >< '/' >< name -> url;
        until (rep() ->> line) = termin do
            insert_contents_links(line,new_index,contents_list,url,system) -> list -> new_index -> contents_list;
            ;;;insert_html_links(system,line) -> list;
            insert_html_tags(list) -> list;
            pr_line(list);
            quitif(issubstring('Files',line));
        enduntil;

        unless (rep() ->> line) = termin do
            insert_contents_links(line,new_index,contents_list,url,system) -> list -> new_index -> contents_list;
            ;;;insert_html_links(system,line) -> list;
            insert_html_tags(list) -> list;
            pr_line(list);
        endunless;

        until (rep() ->> line) = termin or not(line = nullstring) do nl(1); enduntil;

        if line == termin then
           pr('</BODY>\n</HTML>\n');
           return();
	endif;

        for c from `\[b]a` to `\[b]z` do
            consstring(c,1) -> s;
            pr('<A HREF="'><url><'#'><s><'">'><s><'</A> ');
        endfor; nl(2);

        false -> c;
        repeat
            insert_index_links(system,type,line) -> list;
            insert_html_tags(list) -> list;
            unless  line(1) == c then
                line(1) -> c;
                consstring(c,1) -> s;
                ;;;pr('<A NAME="'><system><'/'><type><'/'><name><'#'><s><'"></A>');
                pr('<A NAME="'><s><'"></A>');
            endunless;
            pr_line(list);
        quitif((rep() ->> line) = termin or line = nullstring);
        endrepeat;

        if line == termin then
           pr('</BODY>\n</HTML>\n');
           return();
	endif;

        until (rep() ->> line) = termin do
            insert_contents_links(line,new_index,contents_list,url,system) -> list -> new_index -> contents_list;
            ;;;insert_html_links(system,line) -> list;
            insert_html_tags(list) -> list;
            pr_line(list);
        enduntil;
        pr('</PRE>\n');
    else
        npr('<P>\nFile "'><name><'" not found is system '><system);
	npr('<P>\nUsing list '><list);
    endif;
    pr('</BODY>\n</HTML>\n');
enddefine;

define vars getlibfilename(list, defaultname, name) -> file;
	lvars fextn = false, srchlist, ss_extn;

	if defaultname == "vedlibname"
	and (sys_fname_extn(name) ->> fextn) /= nullstring then
		subsystem_searchlist(defaultname, fextn, true)
	else
		subsystem_searchlist(defaultname, veddocsubsystem, true)
	endif -> (srchlist, ss_extn);

	;;; If libfile with empty extn, use the subsystem extn if there is one
	if fextn = nullstring then
		name sys_>< (ss_extn or pop_default_type)
	else
		name
	endif -> file;
	syssearchpath(srchlist, file, true) -> file;

	;;; Search general list (unless already found)
	unless file then
		if fextn = nullstring then
			name sys_>< if pop_default_type = '.ph' then '.ph' else '.p' endif
		else
			name
		endif -> file;
		syssearchpath(list, file, true) -> file
	endunless;

	if file then
		name -> valof(defaultname)
	endif
enddefine;

define vedtohtml(system,type,name);
    lvars type name file rep list line i index new_index = false, url fext;
    dlocal poplinewidth = false; ;;;stop line breaks
    lvars contents_list = [], contents = false;
    lconstant end_anchor = consword('</A>');

    if system = "pop11" then
        switchon type =
        case 'help'  then vedhelplist  -> list;
        case 'ref'   then vedreflist   -> list;
        case 'teach' then vedteachlist -> list;
        case 'doc'   then veddoclist   -> list;
        case 'lib'   then popuseslist -> list;
        case 'ploghelp'  then subsystem_searchlist("vedhelpname","prolog"->>system)  -> list;
        else
            npr('Invalid Type "'><type><'" expected help, ref, teach or doc');
            return();
        endswitchon;
    else
        switchon type =
        case 'help'  then "vedhelpname"  -> list;
        case 'ref'   then "vedrefname"   -> list;
        case 'teach' then "vedteachname" -> list;
        case 'doc'   then "veddocname"   -> list;
        case 'ploghelp'  then "vedhelpname"  -> list;
        else
            npr('Invalid Type "'><type><'" expected help, ref, teach or doc');
            return();
        endswitchon;
        subsystem_searchlist(list,system) -> list;
    endif;

    syssearchpath(list,name) -> file;
    unless file then syssearchpath(list,lowertoupper(name)) -> file; endunless;
    unless file then syssearchpath(list,uppertolower(name)) -> file; endunless;
    
    if not(file) and type /== 'lib' and (sys_fname_extn(name) ->> fext) == nullstring then
		switchon system ==
		case "pop11"  then '.p' -> fext;
		case "poplog" then '.pl' -> fext;
		case "list"   then '.lsp' -> fext;
		case "pml"    then '.ml' -> fext;
        endswitchon;
		syssearchpath(list,name >< fext) -> file;
    	unless file then syssearchpath(vedsrclist,name >< fext) -> file; endunless;
    	unless file then syssearchpath(vedsrclist,name >< '.s') -> file; endunless;

	endif;


    pr('<HTML>\n<HEAD>\n<TITLE>\n');spr(system);spr(type);npr(name);
    pr('</TITLE>\n');
    pr('<BASE HREF="');pr(base_url);pr('">\n');
    pr('</HEAD>\n<BODY>\n');

    if file then
        if file.islist then hd(file) -> file ; endif;

        vedfile_line_repeater(file,false) -> rep;

        pr('<PRE width=72>\n');

	    system >< '/' >< type >< '/' >< name -> url;

        for line from_repeater rep do
            insert_contents_links(line,new_index,contents_list,url,system) -> line -> new_index -> contents_list;
            insert_html_tags(line) -> line;
            pr_line(line);
        endfor;
        pr('</PRE>\n');
    else
        npr('<P>\nFile "'><name><'" not found is system '><system);
		npr('<P>\nUsing list '><list);
    endif;
    pr('</BODY>\n</HTML>\n');
enddefine;



define inet_addr(s) -> addr;
	lvars s, i j addr = 0;
	1 -> i;
	while(locchar(`.`,i,s) ->> j) do
		if (strnumber(substring(i,j-i,s)) ->> i) then
			addr*256+i -> addr;
		else
			mishap(s,1,'Not a valid inet address');
		endif;
		j + 1 -> i;
	endwhile;
	if (strnumber(substring(i,length(s)-i+1,s)) ->> i) then
		addr*256+i -> addr;
	else
		mishap(s,1,'Not a valid inet address');
	endif;
enddefine;

define parse_addr(s);
lvars s,i,mask;
	if (locchar(`:`,1,s) ->> i) then	;;;has a netmask
		 [% inet_addr(substring(1,i-1,s)), inet_addr(substring(i+1,length(s)-i,s)) %];
	else
		inet_addr(s) -> i;
		if i < #_<inet_addr('127.255.255.255')>_# then		;;;class A
			#_<inet_addr('0.255.255.255')>_# -> mask;
		elseif i < #_<inet_addr('191.255.255.255')>_# then	;;;class B
			#_<inet_addr('0.0.255.255')>_# -> mask;
		elseif i < #_<inet_addr('223.255.255.255')>_# then	;;;class C
			#_<inet_addr('0.0.0.255')>_# -> mask;
		else												;;;class D
			#_<inet_addr('0.0.0.0')>_#  -> mask;
		endif;
		if i&&mask = 0 then
			[%i,mask%]            ;;;network address and mask
		else
			i;                    ;;;host address
		endif;
	endif;
enddefine;

#_IF DEF POPC_COMPILING
[
	['$usepop/pop/x/ved/help/' 	help]
	['$usepop/pop/x/ved/ref/' 	ref]
	['$usepop/pop/x/ved/teach/' teach]
	['$usepop/pop/x/pop/help/' 	help]
	['$usepop/pop/x/pop/ref/' 	ref]
	['$usepop/pop/x/pop/teach/' teach]
	['$usepop/pop/x/pop/doc/' 	doc]
	['$poplocal/local/help/' 	help]
	['$usepop/pop/help/' 		help]
	['$poplocal/local/teach/' 	teach]
	['$usepop/pop/teach/' 		teach]
	['$usepop/pop/doc/' 		doc]
	['$usepop/pop/ref/' 		ref]
	['$usepop/pop/lib/proto/objectclass/help' help]
	['$usepop/pop/lib/proto/objectclass/teach' teach]
	['$usepop/pop/lib/proto/objectclass/ref' ref]
] -> vedhelplist;
[
	['$usepop/pop/x/ved/ref/' 	ref]
	['$usepop/pop/x/ved/help/' 	help]
	['$usepop/pop/x/ved/teach/' teach]
	['$usepop/pop/x/pop/ref/' 	ref]
	['$usepop/pop/x/pop/help/' 	help]
	['$usepop/pop/x/pop/teach/' teach]
	['$usepop/pop/x/pop/doc/' 	doc]
	['$poplocal/local/ref/' 	ref]
	['$usepop/pop/ref/' 		ref]
	['$usepop/pop/doc/' 		doc]
	['$usepop/pop/help/' 		help]
	['$usepop/pop/teach/' 		teach]
	['$usepop/pop/lib/proto/objectclass/ref' ref]
	['$usepop/pop/lib/proto/objectclass/help' help]
	['$usepop/pop/lib/proto/objectclass/teach' teach]
] -> vedreflist;
[
	['$usepop/pop/x/ved/teach/' teach]
	['$usepop/pop/x/ved/help/' 	help]
	['$usepop/pop/x/ved/ref/' 	ref]
	['$usepop/pop/x/pop/teach/' teach]
	['$usepop/pop/x/pop /help/' help]
	['$usepop/pop/x/pop/ref/' 	ref]
	['$usepop/pop/x/pop/doc/' 	doc]
	['$poplocal/local/teach/' 	teach]
	['$usepop/pop/teach/' 		teach]
	['$poplocal/local/help/' 	help]
	['$usepop/pop/help/' 		help]
	['$usepop/pop/doc/' 		doc]
	['$usepop/pop/ref/' 		ref]
	['$usepop/pop/lib/proto/objectclass/teach' teach]
	['$usepop/pop/lib/proto/objectclass/help' help]
	['$usepop/pop/lib/proto/objectclass/ref' ref]
] -> vedteachlist;
[
	['$usepop/pop/x/pop/doc/' 	doc]
	['$usepop/pop/x/pop/help/' 	help]
	['$usepop/pop/x/pop/ref/' 	ref]
	['$usepop/pop/x/pop/teach/' teach]
	['$poplocal/local/doc/' 	doc]
	['$usepop/pop/doc/' 		doc]
	['$usepop/pop/ref/' 		ref]
	['$usepop/pop/help/' 		help]
] -> veddoclist;

[
	'$poplocal/local/lib/'
	'$poplocal/local/auto/'
	'$poplocal/local/auto/'
	'$usepop/pop/lib/sun/'
    '$usepop/pop/lib/auto/'
	'$usepop/pop/lib/ved/'
    '$usepop/pop/lib/ved/term/'
    '$usepop/pop/lib/database/'
    '$usepop/pop/x/pop/auto/'
    '$usepop/pop/x/ui/lib/'
    '$usepop/pop/x/ved/auto/'
    '$poplocal/local/lib/'
    '$popliblib/'
    '$popdatalib/'
    '$usepop/pop/x/pop/lib/'
    '$usepop/pop/x/pop/lib/Xpw/'
	'$usepop/pop/x/pop/lib/Xol/'
    '$usepop/pop/x/ved/lib/'
] -> popuseslist;
[
	['$popsrc/'              src]
	['$usepop/pop/ved/src/'  src]
	['$usepop/pop/lisp/src/' src]
	['$usepop/pop/pml/src/'  src]
	['$usepop/pop/plog/src/' src]
] -> vedsrclist;
#_ENDIF

define cgi_to_vedhtml();
lvars type, name, system i, j, path = systranslate('PATH_INFO');
/*
dlocal prmishap = procedure(message,list);
        sysprmessage(destlist(list),message,'CGI Error -',0);
    endprocedure;
*/
dlocal base_url;
	if (systranslate('$usepop_base_url') ->> i) then
		i -> base_url;
	endif;

    npr('Content-type: text/html');
    nl(1);
    if  (locchar(`/`,2,path) ->> i) then
        consword(substring(2,i-2,path)) -> system;
        if (locchar(`/`,i+1,path) ->> j) then
            substring(i+1,j-i-1,path) -> type;
            substring(j+1,length(path)-j,path) -> name;
        else
            substring(i+1,length(path)-i,path) -> name;
            if system = "man" or system = "unix" then
                nullstring -> type;                                ;;; section
            else
                substring(2,i-2,path) -> type;
                "pop11" -> system;
            endif;
        endif;

        switchon system =
        case "pop11"  then
            if type = 'man' or type = 'unix' then
                "man" -> system;
                nullstring -> type;
            endif;
#_IF DEF POPC_COMPILING
#_ELSE
        case "lisp"   then useslib('lisp_subsystem');
        case "ml"     then useslib('ml_subsystem');
        case "prolog" then useslib('prolog_subsystem');
#_ENDIF
        endswitchon;

        if system = "man" or system = "unix" then
/*
            if hasstartstring(systranslate('REMOTE_ADDR'),'192.133.244')
            or hasstartstring(systranslate('REMOTE_ADDR'),'134.225')
            then mantohtml(type,name,false);
            else
                npr('Sorry no access to manual pages outside The University of Reading');
            endif
*/
            mantohtml(type,name,false);
        elseif issubstring('ndex',name) then
            indextohtml(system,type,name);
        else
            ;;;indextohtml(system,type,name);
            vedtohtml(system,type,name);
        endif;
    else
        npr('<HTML><P>Invlaid path "'><path><'"</HTML>');
    endif;
enddefine;

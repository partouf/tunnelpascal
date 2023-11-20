{%MainUnit sysutils.pp}
{
  This file is in the public domain.
}

{
  This codepage information is based on what is documented at
    1. http://wiki.whatwg.org/wiki/Web_Encodings
    2. http://msdn.microsoft.com/en-us/library/windows/desktop/dd317756(v=vs.85).aspx

    This file only uses bare facts and API information from the above pages
    rather than descriptions or code, neither of which are copyrightable.
}

{
  Program that generates the rest of the file from the "InputCodepages" array:
}

(*
{$mode objfpc} {$longstrings on} {$typedaddress on} {$coperators on} {$zerobasedstrings on}
{$modeswitch advancedrecords} {$modeswitch arrayoperators} {$modeswitch anonymousfunctions}
uses
	SysUtils, Math;

type
	InputCodepageRec = record
		cp: TSystemCodePage;
		names: array of string;
	end;

// First name is returned by CodePageToCodePageName (with letter case preserved).
// Following names are alternative names recognized by CodePageNameToCodePage (in addition to primary).
// Add DisableName2Cp to prevent CodePageNameToCodePage from knowing a particular name (to eliminate a duplicate).
const
	DisableName2Cp = '<DisableName2Cp>';
	InputCodepages: array of InputCodepageRec =
	(
		(cp: 37; names: ('ibm037', 'cp037', 'ebcdic-cp-ca', 'ebcdic-cp-wt', 'ebcdic-cp-us', 'ebcdic-cp-nl', 'csibm037')),
		(cp: 437; names: ('ibm437', '437', 'cp437', 'cspc8codepage437')),
		(cp: 500; names: ('IBM500', 'cp500', 'ebcdic-cp-ch', 'ebcdic-cp-be', 'csibm500')),
		(cp: 708; names: ('asmo-708')),
		(cp: 720; names: ('DOS-720')),
		(cp: 737; names: ('ibm737')),
		(cp: 775; names: ('ibm775')),
		(cp: 850; names: ('ibm850', 'cp850')),
		(cp: 852; names: ('ibm852', 'cp852')),
		(cp: 855; names: ('IBM855', 'cp855')),
		(cp: 856; names: ('cp856')),
		(cp: 857; names: ('ibm857', 'cp857')),
		(cp: 858; names: ('ibm00858', 'cp858', 'pc-multilingual-850+euro', 'cp00858', 'ccsid00858')),
		(cp: 860; names: ('IBM860', 'cp860')),
		(cp: 861; names: ('ibm861', 'cp861')),
		(cp: 862; names: ('DOS-862', 'cp862', 'ibm862')),
		(cp: 863; names: ('IBM863', 'cp863')),
		(cp: 864; names: ('IBM864', 'cp864')),
		(cp: 865; names: ('IBM865', 'cp865')),
		(cp: 866; names: ('cp866', 'ibm866')),
		(cp: 869; names: ('ibm869', 'cp869')),
		(cp: 870; names: ('IBM870', 'cp870', 'ebcdic-cp-yu', 'ebcdic-cp-roece', 'csibm870')),
		(cp: 874; names: ('windows-874', 'tis-620', 'dos-874')),
		(cp: 875; names: ('cp875')),
		(cp: 932; names: ('shift_jis', 'sjis', 'windows-31j', 'x-sjis', 'x-ms-cp932', 'ms_kanji', 'csshiftjis', 'shift-jis', 'cswindows31j')),
		(cp: 936; names: ('gb2312', 'gbk', 'cn-gb', 'iso-ir-58', 'csgb231280', 'gb231280', 'gb_2312-80', 'gb2312-80', 'csgb2312', 'chinese', 'csiso58gb231280')),
		(cp: 949; names: ('ks_c_5601-1987', 'ksc5601', 'ks-c-5601', 'csksc56011987', 'iso-ir-149', 'ks_c_5601', 'korean', 'ks-c5601', 'ksc_5601', 'ks_c_5601-1989', 'ks_c_5601_1987')),
		(cp: 950; names: ('big5', 'x-x-big5', 'csbig5', 'cn-big5', 'big5-hkscs')),
		(cp: 1026; names: ('ibm1026', 'cp1026', 'csibm1026')),
		(cp: 1047; names: ('ibm01047')),
		(cp: 1140; names: ('ibm01140', 'cp01140', 'ebcdic-us-37+euro', 'ccsid01140')),
		(cp: 1141; names: ('IBM01141', 'cp01141', 'ebcdic-de-273+euro', 'ccsid01141')),
		(cp: 1142; names: ('IBM01142', 'ebcdic-no-277+euro', 'ebcdic-dk-277+euro', 'cp01142', 'ccsid01142')),
		(cp: 1143; names: ('IBM01143', 'ebcdic-se-278+euro', 'ebcdic-fi-278+euro', 'cp01143', 'ccsid01143')),
		(cp: 1144; names: ('IBM01144', 'cp01144', 'ebcdic-it-280+euro', 'ccsid01144')),
		(cp: 1145; names: ('ibm01145', 'cp01145', 'ebcdic-es-284+euro', 'ccsid01145')),
		(cp: 1146; names: ('ibm01146', 'cp01146', 'ebcdic-gb-285+euro', 'ccsid01146')),
		(cp: 1147; names: ('ibm01147', 'cp01147', 'ccsid01147', 'ebcdic-fr-297+euro')),
		(cp: 1148; names: ('IBM01148', 'ebcdic-international-500+euro', 'cp01148', 'ccsid01148')),
		(cp: 1149; names: ('IBM01149', 'cp01149', 'ebcdic-is-871+euro', 'ccsid01149')),
		(cp: 1200; names: ('utf-16', 'ucs-2', 'utf-16le', 'unicode', 'iso-10646-ucs-2')),
		(cp: 1201; names: ('unicodefffe', 'utf-16be')),
		(cp: 1250; names: ('windows-1250', 'x-cp1250')),
		(cp: 1251; names: ('windows-1251', 'x-cp1251')),
		(cp: 1252; names: ('windows-1252', 'x-ansi')),
		(cp: 1253; names: ('windows-1253')),
		(cp: 1254; names: ('windows-1254')),
		(cp: 1255; names: ('windows-1255')),
		(cp: 1256; names: ('windows-1256', 'cp1256')),
		(cp: 1257; names: ('windows-1257')),
		(cp: 1258; names: ('windows-1258')),
		(cp: 1361; names: ('Johab')),
		(cp: 3021; names: ('MIK', 'cp3021')),
		(cp: 10000; names: ('macintosh')),
		(cp: 10001; names: ('x-mac-japanese')),
		(cp: 10002; names: ('x-mac-chinesetrad')),
		(cp: 10003; names: ('x-mac-korean')),
		(cp: 10004; names: ('x-mac-arabic')),
		(cp: 10005; names: ('x-mac-hebrew')),
		(cp: 10006; names: ('x-mac-greek')),
		(cp: 10007; names: ('x-mac-cyrillic')),
		(cp: 10008; names: ('x-mac-chinesesimp')),
		(cp: 10010; names: ('x-mac-romanian')),
		(cp: 10017; names: ('x-mac-ukrainian')),
		(cp: 10021; names: ('x-mac-thai')),
		(cp: 10029; names: ('x-mac-ce')),
		(cp: 10079; names: ('x-mac-icelandic')),
		(cp: 10081; names: ('x-mac-turkish')),
		(cp: 10082; names: ('x-mac-croatian')),
		(cp: 12000; names: ('utf-32')),
		(cp: 12001; names: ('utf-32BE')),
		(cp: 20000; names: ('x-Chinese_CNS', 'x-chinese-cns')),
		(cp: 20001; names: ('x-cp20001')),
		(cp: 20002; names: ('x_Chinese-Eten', 'x-chinese-eten')),
		(cp: 20003; names: ('x-cp20003')),
		(cp: 20004; names: ('x-cp20004')),
		(cp: 20005; names: ('x-cp20005')),
		(cp: 20105; names: ('x-IA5', 'irv')),
		(cp: 20106; names: ('x-ia5-german', 'din_66003', 'german')),
		(cp: 20107; names: ('x-IA5-Swedish', 'swedish', 'sen_850200_b')),
		(cp: 20108; names: ('x-IA5-Norwegian', 'ns_4551-1', 'norwegian')),
		(cp: 20127; names: ('us-ascii', 'us', 'ascii', 'cp367', 'ansi_x3.4-1968', 'ansi_x3.4-1986', 'iso646-us', 'ibm367', 'iso_646.irv:1991', 'iso-ir-6', 'csascii')),
		(cp: 20261; names: ('x-cp20261')),
		(cp: 20269; names: ('x-cp20269')),
		(cp: 20273; names: ('ibm273', 'cp273', 'csibm273')),
		(cp: 20277; names: ('ibm277', 'ebcdic-cp-dk', 'ebcdic-cp-no', 'csibm277')),
		(cp: 20278; names: ('ibm278', 'cp278', 'ebcdic-cp-fi', 'ebcdic-cp-se', 'csibm278')),
		(cp: 20280; names: ('ibm280', 'cp280', 'ebcdic-cp-it', 'csibm280')),
		(cp: 20284; names: ('ibm284', 'cp284', 'ebcdic-cp-es', 'csibm284')),
		(cp: 20285; names: ('IBM285', 'cp285', 'ebcdic-cp-gb', 'csibm285')),
		(cp: 20290; names: ('IBM290', 'cp290', 'csibm290', 'ebcdic-jp-kana')),
		(cp: 20297; names: ('IBM297', 'cp297', 'ebcdic-cp-fr', 'csibm297')),
		(cp: 20420; names: ('ibm420', 'cp420', 'csibm420', 'ebcdic-cp-ar1')),
		(cp: 20423; names: ('ibm423', 'cp423', 'ebcdic-cp-gr', 'csibm423')),
		(cp: 20424; names: ('IBM424', 'cp424', 'ebcdic-cp-he', 'csibm424')),
		(cp: 20833; names: ('x-EBCDIC-KoreanExtended')),
		(cp: 20838; names: ('ibm-thai', 'csibmthai')),
		(cp: 20866; names: ('koi8-r', 'koi', 'koi8', 'koi8r', 'cskoi8r')),
		(cp: 20871; names: ('ibm871', 'cp871', 'ebcdic-cp-is', 'csibm871')),
		(cp: 20880; names: ('ibm880', 'cp880', 'csibm880', 'ebcdic-cyrillic')),
		(cp: 20905; names: ('ibm905', 'cp905', 'ebcdic-cp-tr', 'csibm905')),
		(cp: 20924; names: ('IBM00924', 'ebcdic-latin9--euro', 'cp00924', 'ccsid00924')),
		(cp: 20932; names: ('EUC-JP')),
		(cp: 20936; names: ('x-cp20936')),
		(cp: 20949; names: ('x-cp20949')),
		(cp: 21025; names: ('cp1025')),
		(cp: 21027; names: ('x-cp21027')),
		(cp: 21866; names: ('koi8-u', 'koi8-ru')),
		(cp: 28591; names: ('iso-8859-1', 'l1', 'cp819', 'iso-ir-100', 'iso8859-1', 'iso_8859-1', 'csisolatin1', 'ibm819', 'latin1', 'iso_8859-1:1987')),
		(cp: 28592; names: ('iso-8859-2', 'l2', 'iso-ir-101', 'iso8859-2', 'iso_8859-2', 'csisolatin2', 'latin2', 'iso_8859-2:1987')),
		(cp: 28593; names: ('iso-8859-3', 'l3', 'iso-ir-109', 'iso_8859-3', 'csisolatin3', 'latin3', 'iso_8859-3:1988')),
		(cp: 28594; names: ('iso-8859-4', 'l4', 'iso-ir-110', 'iso_8859-4', 'csisolatin4', 'latin4', 'iso_8859-4:1988')),
		(cp: 28595; names: ('iso-8859-5', 'iso-ir-144', 'iso_8859-5', 'iso_8859-5:1988', 'csisolatincyrillic', 'cyrillic')),
		(cp: 28596; names: ('iso-8859-6', 'iso-ir-127', 'iso_8859-6', 'arabic', 'iso_8859-6:1987', 'ecma-114', 'csisolatinarabic')),
		(cp: 28597; names: ('iso-8859-7', 'greek', 'elot_928', 'iso-ir-126', 'iso_8859-7', 'csisolatingreek', 'greek8', 'iso_8859-7:1987', 'ecma-118')),
		(cp: 28598; names: ('iso-8859-8', 'logical', 'iso-ir-138', 'iso_8859-8', 'hebrew', 'iso_8859-8:1988', 'visual', 'csisolatinhebrew', 'iso-8859-8 visual')),
		(cp: 28599; names: ('iso-8859-9', 'l5', 'iso-ir-148', 'iso_8859-9', 'csisolatin5', 'latin5', 'iso_8859-9:1989')),
		(cp: 28600; names: ('iso-8859-10')),
		(cp: 28601; names: ('iso-8859-11')),
		(cp: 28603; names: ('iso-8859-13')),
		(cp: 28604; names: ('iso-8859-14')),
		(cp: 28605; names: ('iso-8859-15', 'l9', 'csisolatin9', 'latin9', 'iso_8859-15')),
		(cp: 28606; names: ('iso-8859-16')),
		(cp: 29001; names: ('x-Europa')),
		(cp: 38598; names: ('iso-8859-8-i')),
		(cp: 50220; names: ('iso-2022-jp')),
		(cp: 50221; names: ('csISO2022JP')),
		(cp: 50222; names: ('iso-2022-jp' + DisableName2Cp)),
		(cp: 50225; names: ('iso-2022-kr', 'iso-2022-kr-7bit', 'csiso2022kr', 'iso-2022-kr-7')),
		(cp: 50227; names: ('x-cp50227')),
		(cp: 50229; names: ('x-cp50229')),
		(cp: 50930; names: ('cp930')),
		(cp: 50931; names: ('x-ebcdic-japaneseanduscanada')),
		(cp: 50933; names: ('cp933')),
		(cp: 50935; names: ('cp935')),
		(cp: 50937; names: ('cp937')),
		(cp: 50939; names: ('cp939')),
		(cp: 51932; names: ('euc-jp' + DisableName2Cp, 'x-euc', 'x-euc-jp', 'extended_unix_code_packed_format_for_japanese', 'cseucpkdfmtjapanese', 'iso-2022-jpeuc')),
		(cp: 51936; names: ('euc-cn', 'x-euc-cn')),
		(cp: 51949; names: ('euc-kr', 'iso-2022-kr-8bit', 'cseuckr', 'iso-2022-kr-8')),
		(cp: 52936; names: ('hz-gb-2312')),
		(cp: 54936; names: ('gb18030')),
		(cp: 57002; names: ('x-iscii-de')),
		(cp: 57003; names: ('x-iscii-be')),
		(cp: 57004; names: ('x-iscii-ta')),
		(cp: 57005; names: ('x-iscii-te')),
		(cp: 57006; names: ('x-iscii-as')),
		(cp: 57007; names: ('x-iscii-or')),
		(cp: 57008; names: ('x-iscii-ka')),
		(cp: 57009; names: ('x-iscii-ma')),
		(cp: 57010; names: ('x-iscii-gu')),
		(cp: 57011; names: ('x-iscii-pa')),
		(cp: 65000; names: ('utf-7', 'unicode-1-1-utf-7', 'csunicode11utf7', 'unicode-2-0-utf-7', 'x-unicode-2-0-utf-7', 'x-unicode-1-1-utf-7')),
		(cp: 65001; names: ('utf-8', 'unicode-1-1-utf-8', 'unicode-2-0-utf-8', 'x-unicode-2-0-utf-8', 'x-unicode-1-1-utf-8'))
	);

type
	NameRec = record
		name: string;
		disableName2Cp: boolean;
	end;

	function ParseName(const name: string): NameRec;
	var
		p: SizeInt;
	begin
		result.name := name;
		p := Pos(DisableName2Cp, result.name);
		result.disableName2Cp := p > 0;
		if result.disableName2Cp then delete(result.name, p, length(DisableName2Cp));
	end;

	function BakeIntoString(var s: string; const piece: string): SizeInt;
	begin
		result := Pos(piece, s) - 1;
		if result >= 0 then exit;
		result := length(s);
		s += piece;
	end;

type
	Prefixes = record
		prefixStarts: array of uint32;
		data: string;
		procedure Init(const src: array of string);
		class function CommonPrefixLen(const a, b: string): SizeUint; static;
	end;

	procedure Prefixes.Init(const src: array of string);
	var
		iA, iB, iC, pl, curScore, bestScore: SizeInt;
		names: array of string;
		curPrefix, bestPrefix: string;
	begin
		SetLength(prefixStarts, length(src));
		FillChar(pUint32(prefixStarts)^, length(prefixStarts) * sizeof(uint32), byte(-1));
		names := Copy(src);
		repeat
			bestScore := 0;
			for iA := 0 to High(names) - 1 do
				for iB := iA + 1 to High(names) do
				begin
					pl := CommonPrefixLen(names[iA], names[iB]);
					if pl >= 2 then
					begin
						curScore := 2;
						curPrefix := Copy(names[iA], 1, pl);
						for iC := iB + 1 to High(names) do
							if names[iC].StartsWith(curPrefix) then curScore += 1;
						curScore := pl + curScore * (pl - 2);
						if curScore > bestScore then
						begin
							bestScore := curScore;
							bestPrefix := curPrefix;
						end;
					end;
				end;
			if bestScore = 0 then break;

			for iA := 0 to High(names) do
				if names[iA].StartsWith((@bestPrefix)^) then
				begin
					prefixStarts[iA] := length(data);
					names[iA] := '';
				end;
			data += (@bestPrefix)^ + #0;
		until false;
		if data = '' then data := #0;
		for iA := 0 to High(names) do
			if int32(prefixStarts[iA]) = -1 then prefixStarts[iA] := Pos(#0, data) - 1;
	end;

	class function Prefixes.CommonPrefixLen(const a, b: string): SizeUint;
	begin
		result := 0;
		while (result < SizeUint(min(length(a), length(b)))) and (a[result] = b[result]) do result += 1;
	end;

type
	Cp2Name = record
		h: array of record
			flatStart, count: uint32;
		end;
		flat: array of record
			cp: TSystemCodePage;
			nameStart, inputCp2IFlat: uint32;
		end;
		flatNames: string;
		inputCp2Flat: array of uint32;
		px: Prefixes;
		procedure Init;
		function Find(cp: TSystemCodePage): string;
	end;

	procedure Cp2Name.Init;
	var
		iIc, ih, ihc, iFlat: SizeInt;
		hb: array of record
			inputCps: array of uint32;
		end;
		flatNameSrc: array of string;
	begin
		SetLength((@hb)^, 1 shl max(1, integer(BsrDWord(length(InputCodepages))) - 3));
		SetLength(h, length(hb));
		for iIc := 0 to High(InputCodepages) do
			Insert(iIc, hb[InputCodepages[iIc].cp and High(hb)].inputCps, High(SizeInt));
		SetLength(flat, length(InputCodepages));
		SetLength((@flatNameSrc)^, length(InputCodepages));
		flatNames := '';
		iFlat := 0;
		for ih := 0 to High(hb) do
		begin
			h[ih].flatStart := iFlat;
			h[ih].count := length(hb[ih].inputCps);
			for ihc := 0 to High(hb[ih].inputCps) do
			begin
				flatNameSrc[iFlat] := ParseName(InputCodepages[hb[ih].inputCps[ihc]].names[0]).name;
				flat[iFlat].cp := InputCodepages[hb[ih].inputCps[ihc]].cp;
				flat[hb[ih].inputCps[ihc]].inputCp2IFlat := iFlat;
				iFlat += 1;
			end;
		end;
		px.Init(flatNameSrc);
		iFlat := 0;
		for ih := 0 to High(hb) do
			for ihc := 0 to High(hb[ih].inputCps) do
			begin
				flat[iFlat].nameStart := BakeIntoString(flatNames, Copy(flatNameSrc[iFlat], 1 + length(@px.data[px.prefixStarts[iFlat]])) + #0);
				iFlat += 1;
			end;
	end;

	function Cp2Name.Find(cp: TSystemCodePage): string;
	var
		i: SizeInt;
	begin
		for i := 0 to SizeInt(h[cp and High(h)].count - 1) do
			if flat[h[cp and High(h)].flatStart + SizeUint(i)].cp = cp then
				exit(string(pChar(@px.data[px.prefixStarts[h[cp and High(h)].flatStart + SizeUint(i)]])) +
					pChar(@flatNames[flat[h[cp and High(h)].flatStart + SizeUint(i)].nameStart]));
		result := '';
	end;

type
	TrieA = record
	type
		Vertex = record
			parentChar: char;
			childs: array of uint32;
			word: uint32;
		end;
	var
		v: array of Vertex;
		nv: SizeUint;
		procedure Init;
		procedure Add(const s: string; word: SizeUint);
	end;

	procedure TrieA.Init;
	var
		iIc, iName, totalChars: SizeInt;
	begin
		totalChars := 0;
		for iIc := 0 to High(InputCodepages) do
			for iName := 0 to High(InputCodepages[iIc].names) do
				totalChars += length(InputCodepages[iIc].names[iName]);

		SetLength(v, 1 + totalChars);
		v[0].parentChar := '-';
		nv := 1;

		for iIc := 0 to High(InputCodepages) do
			for iName := 0 to High(InputCodepages[iIc].names) do
				if not ParseName(InputCodepages[iIc].names[iName]).disableName2Cp then
					Add(LowerCase(ParseName(InputCodepages[iIc].names[iName]).name), 1 + iIc);
	end;

	procedure TrieA.Add(const s: string; word: SizeUint);
	var
		iChar, curVertex, iChild: SizeInt;
		childFound: boolean;
	begin
		curVertex := 0;
		for iChar := 0 to High(s) do
		begin
			childFound := false;
			for iChild := 0 to High(v[curVertex].childs) do
				if s[iChar] = v[v[curVertex].childs[iChild]].parentChar then
				begin
					curVertex := v[curVertex].childs[iChild];
					childFound := true;
					break;
				end;
			if childFound then
				continue;
			Insert(nv, v[curVertex].childs, High(SizeInt));
			v[nv].parentChar := s[iChar];
			curVertex := nv;
			nv += 1;
		end;
		v[curVertex].word := word;
	end;

type
	TrieB = record
	type
		Vertex = record
			word: uint32;
		case sequence: boolean of
			false: (firstChild, nChilds: uint32);
			true: (seqChild, iSeq: uint32);
		end;
		Sequence = record
			firstChar: uint32;
		end;
	var
		v: array of Vertex;
		sequences: array of Sequence;
		parentChars: string; // if v[i].sequence = true, parentChars[v[i].seqChild] is used as chr(count of characters in sequence).
		seqChars: string;
		procedure Init(var ta: TrieA);
		class function DetectSequence(var ta: TrieA; iv: SizeUint; out s: string): SizeInt; static;
		function Find(const name: string): SizeInt;
	end;

	procedure TrieB.Init(var ta: TrieA);
	var
		q: array of uint32;
		qs, qe, iSeq: SizeUint;
		ivseqe, iChild: SizeInt;
		seqs: string;
	begin
		SetLength(v, ta.nv);
		SetLength(parentChars, ta.nv);
		parentChars[0] := '-';
		SetLength((@q)^, ta.nv);
		q[0] := 0;
		qs := 0;
		qe := 1;
		repeat
			v[qs].word := ta.v[q[qs]].word;
			ivseqe := DetectSequence(ta, q[qs], seqs);
			if ivseqe >= 0 then
			begin
				v[qs].sequence := true;
				iSeq := length(sequences);
				SetLength(sequences, iSeq + 1);
				v[qs].seqChild := qe;
				v[qs].iSeq := iSeq;
				sequences[iSeq].firstChar := BakeIntoString(seqChars, seqs);
				parentChars[qe] := chr(length(seqs));
				q[qe] := ivseqe;
				qe += 1;
			end else
			begin
				v[qs].firstChild := qe;
				v[qs].nChilds := length(ta.v[q[qs]].childs);
				Move(pUint32(ta.v[q[qs]].childs)^, pUint32(q)[qe], v[qs].nChilds * sizeof(uint32));
				for iChild := 0 to SizeInt(v[qs].nChilds) - 1 do
					parentChars[SizeInt(qe) + iChild] := ta.v[ta.v[q[qs]].childs[iChild]].parentChar;
				qe += SizeUint(length(ta.v[q[qs]].childs));
			end;
			qs += 1;
		until qs = qe;
		SetLength(v, qe);
		SetLength(parentChars, qe);
	end;

	class function TrieB.DetectSequence(var ta: TrieA; iv: SizeUint; out s: string): SizeInt;
	begin
		s := '';
		while length(ta.v[iv].childs) = 1 do
		begin
			iv := ta.v[iv].childs[0];
			s += ta.v[iv].parentChar;
			if ta.v[iv].word > 0 then break;
		end;
		if length(s) >= 3 then result := iv else result := -1;
	end;

	function TrieB.Find(const name: string): SizeInt;
	var
		iv, np, nn, nSeq: SizeUint;
		iiv: SizeInt;
		nameLo: string;
	begin
		nameLo := LowerCase(name);
		iv := 0;
		np := 0;
		nn := length(name);
		repeat
			if np = nn then exit(v[iv].word - 1);
			if v[iv].sequence then
			begin
				nSeq := ord(parentChars[v[iv].seqChild]);
				if (nn - np < nSeq) or (CompareByte(nameLo[np], seqChars[sequences[v[iv].iSeq].firstChar], nSeq) <> 0) then
					exit(-1);
				np += nSeq;
				iv := v[iv].seqChild;
			end else
			begin
				iiv := IndexByte(parentChars[v[iv].firstChild], v[iv].nChilds, ord(nameLo[np]));
				if iiv < 0 then exit(-1);
				iv := v[iv].firstChild + SizeUint(iiv);
				np += 1;
			end;
		until false;
	end;

	procedure Fail(const msg: string);
	begin
		writeln(msg);
		halt(1);
	end;

const
	LE = LineEnding;
	LineWidth = 150;

type
	GetIntProc = function(id: SizeUint; param: pointer): SizeUint;

	function PascalIntArray(const name: string; n: SizeUint; getInt: GetIntProc; param: pointer;
		const forceType: string = ''): string;
	var
		v, maxval: SizeUint;
		i, linestart, maybeMoveBracketToNewLine: SizeInt;
		typename: string;
	begin
		typename := forceType;
		if typename = '' then
		begin
			maxval := 0;
			for i := 0 to SizeInt(n) - 1 do
				maxval := max(maxval, getInt(i, param));
			if maxval <= 255 then typename := 'uint8'
			else if maxval <= 65535 then typename := 'uint16'
			else typename := 'uint32';
		end;

		result := '  ' + name + ': array[0 .. ' + IntToStr(max(SizeInt(n) - 1, 0)) + '] of ' + typename + ' =';
		maybeMoveBracketToNewLine := length(result);
		result += ' (';
		linestart := 0;
		for i := 0 to SizeInt(n) - 1 do
		begin
			v := getInt(i, param);
			result += specialize IfThen<string>(v = 65535, '$FFFF', IntToStr(v)) + specialize IfThen<string>(SizeUint(i) + 1 < n, ',', '');
			if (maybeMoveBracketToNewLine >= 0) and (length(result) - linestart >= LineWidth) then
			begin
				linestart := maybeMoveBracketToNewLine + length(' (') - length(result);
				Insert(LE + '    ', result, 1 + maybeMoveBracketToNewLine + length(' ('));
				Insert(LE + ' ', result, 1 + maybeMoveBracketToNewLine);
				linestart += length(result);
				maybeMoveBracketToNewLine := -1;
			end;
			if length(result) - linestart >= LineWidth then
			begin
				if SizeUint(i) + 1 < n then result += LE + '    ' else result += '';
				linestart := length(result);
			end else
				result += {' '}'';
		end;
		result += specialize IfThen<string>(n = 0, '0', '') + specialize IfThen<string>(maybeMoveBracketToNewLine < 0, LE + '  ', '') + ');';
	end;

	function PascalCharArray(const name, data: string): string;
	var
		quote: boolean;
		i, linestart: SizeInt;
	begin
		result := '  ' + name + ': array[0 .. ' + IntToStr(max(length(data) - 1, 0)) + '] of ansichar =' + LE +
			'    ';
		linestart := length(result);
		quote := false;
		for i := 0 to High(data) do
		begin
			if (data[i] >= #32) and (data[i] <= #127) then
			begin
				if not quote then result += '''';
				result += data[i];
				quote := true;
			end else
			begin
				if quote then result += '''';
				result += '#' + IntToStr(ord(data[i]));
				quote := false;
			end;
			if length(result) - linestart >= LineWidth then
			begin
				if quote then begin result += ''''; quote := false; end;
				if i + 1 < length(data) then result += ' +' + LE + '    ' else result += '';
				linestart := length(result);
			end;
		end;
		result += specialize IfThen<string>(length(data) = 0, '''''', '') + specialize IfThen<string>(quote, '''', '') + ';';
	end;

var
	ta: TrieA;
	tb: TrieB;
	cp2n: Cp2Name;
	iIc, iName, rIc, iv, maxNChilds: SizeInt;
	pn: NameRec;
	foundName, src: string;

begin
	cp2n.Init;
	ta.Init;
	tb.Init(ta);
	for iIc := 0 to High(InputCodepages) do
	begin
		for iName := 0 to High(InputCodepages[iIc].names) do
		begin
			pn := ParseName(InputCodepages[iIc].names[0]);
			if iName = 0 then
			begin
				foundName := cp2n.Find(InputCodepages[iIc].cp); // CodePageToCodePageName(InputCodepages[iIc].cp);
				if foundName <> pn.name then
					Fail('Failed to find codepage ' + IntToStr(InputCodepages[iIc].cp) + ': got "' + foundName + '", expected "' + pn.name + '".');
			end;
			if not ParseName(InputCodepages[iIc].names[iName]).disableName2Cp then
			begin
				rIc := tb.Find(InputCodepages[iIc].names[iName]); // CodePageNameToCodePage(InputCodepages[iIc].names[iName]);
				if rIc <> iIc then // InputCodepages[iIc].cp then
					Fail('Failed to find "' + InputCodepages[iIc].names[iName] + '": got ' + IntToStr(rIc) + ', expected ' + IntToStr(iIc) + '.');
			end;
		end;
	end;

	maxNChilds := 0;
	for iv := 0 to High(tb.v) do
		if not tb.v[iv].sequence then maxNChilds := max(maxNChilds, tb.v[iv].nChilds);

	src :=
		'const' + LE +
		PascalIntArray('Cp2Cpn_HashFlatStart', length(cp2n.h),
			function(id: SizeUint; param: pointer): SizeUint begin result := cp2n.h[id].flatStart; end, nil) +
		LE +
		PascalIntArray('Cp2Cpn_HashFlatCount', length(cp2n.h),
			function(id: SizeUint; param: pointer): SizeUint begin result := cp2n.h[id].count; end, nil) +
		LE +
		PascalIntArray('Cp2Cpn_FlatCp1', 1 + length(cp2n.flat),
			function(id: SizeUint; param: pointer): SizeUint begin if id = 0 then result := $FFFF else result := cp2n.flat[id - 1].cp; end, nil, 'TSystemCodePage') +
		LE +
		PascalIntArray('Cp2Cpn_FlatNameStart', length(cp2n.flat),
			function(id: SizeUint; param: pointer): SizeUint begin result := cp2n.flat[id].nameStart; end, nil) +
		LE +
		PascalCharArray('Cp2Cpn_FlatNames', cp2n.flatNames) +
		LE +
		PascalIntArray('Cp2Cpn_PrefixStart', length(cp2n.flat),
			function(id: SizeUint; param: pointer): SizeUint begin result := cp2n.px.prefixStarts[id]; end, nil) +
		LE +
		PascalCharArray('Cp2Cpn_Prefixes', cp2n.px.data) +
		LE +
		PascalIntArray('Cpn2Cp_ToChilds', length(tb.v),
			function(id: SizeUint; param: pointer): SizeUint
			begin
				if tb.v[id].sequence then
					result := tb.v[id].seqChild - id
				else
					result := tb.v[id].firstChild - id;
			end, nil) +
		LE +
		PascalIntArray('Cpn2Cp_NChildsOrISeq', length(tb.v),
			function(id: SizeUint; param: pointer): SizeUint
			begin
				if tb.v[id].sequence then
					result := maxNChilds + 1 + SizeInt(tb.v[id].iSeq)
				else
					result := tb.v[id].nChilds;
			end, nil) +
		LE +
		PascalCharArray('Cpn2Cp_ParentCharsOrSeqCounts', tb.parentChars) +
		LE +
		PascalIntArray('Cpn2Cp_SeqCharStart', length(tb.sequences),
			function(id: SizeUint; param: pointer): SizeUint begin result := tb.sequences[id].firstChar; end, nil) +
		LE +
		PascalCharArray('Cpn2Cp_SeqChars', tb.seqChars) +
		LE +
		PascalIntArray('Cpn2Cp_IFlatCp1', length(tb.v),
			function(id: SizeUint; param: pointer): SizeUint
			var
				iw: SizeUint;
			begin
				iw := tb.v[id].word;
				if iw = 0 then exit(0);
				result := 1 + cp2n.flat[iw - 1].inputCp2IFlat;
			end, nil) +
		LE +
		LE +
		'function CodePageToCodePageName(cp: TSystemCodePage): AnsiString;' + LE +
		'var' + LE +
		'  fstart, i: SizeInt;' + LE +
		'begin' + LE +
		'  fstart := Cp2Cpn_HashFlatStart[cp and ' + IntToStr(High(cp2n.h)) + '];' + LE +
		'  i := IndexWord(PUint16(Cp2Cpn_FlatCp1)[1 + fstart], Cp2Cpn_HashFlatCount[cp and ' + IntToStr(High(cp2n.h)) + '], cp);' + LE +
		'  if i >= 0 then' + LE +
		'    result := AnsiString(shortstring(PAnsiChar(@Cp2Cpn_Prefixes[Cp2Cpn_PrefixStart[fstart + i]])) + shortstring(PAnsiChar(@Cp2Cpn_FlatNames[Cp2Cpn_FlatNameStart[fstart + i]])))' + LE +
		'  else' + LE +
		'    result := '''';' + LE +
		'end;' + LE +
		LE +
		'function CodePageNameToCodePage(const cpname: AnsiString): TSystemCodePage;' + LE +
		'var' + LE +
		'  cp, ce, seqp: PAnsiChar;' + LE +
		'  iv, iiv, ncis, nseq: SizeInt;' + LE +
		'begin' + LE +
		'  result := $FFFF;' + LE +
		'  cp := PAnsiChar(pointer(cpname));' + LE +
		'  ce := cp + length(cpname);' + LE +
		'  iv := 0;' + LE +
		'  while cp < ce do' + LE +
		'  begin' + LE +
		'    ncis := Cpn2Cp_NChildsOrISeq[iv];' + LE +
		'    iv := iv + Cpn2Cp_ToChilds[iv];' + LE +
		'    if ncis < ' + IntToStr(maxNChilds + 1) + ' then' + LE +
		'    begin' + LE +
		'      iiv := IndexByte(Cpn2Cp_ParentCharsOrSeqCounts[iv], ncis, ord(LowerCase(cp^)));' + LE +
		'      if iiv < 0 then exit;' + LE +
		'      iv := iv + iiv;' + LE +
		'      inc(cp);' + LE +
		'    end else' + LE +
		'    begin' + LE +
		'      seqp := @Cpn2Cp_SeqChars[Cpn2Cp_SeqCharStart[ncis - ' + IntToStr(maxNChilds + 1) + ']];' + LE +
		'      nseq := ord(Cpn2Cp_ParentCharsOrSeqCounts[iv]);' + LE +
		'      if nseq > ce - cp then exit;' + LE +
		'      repeat' + LE +
		'        if LowerCase(cp^) <> seqp^ then exit;' + LE +
		'        inc(cp); inc(seqp); dec(nseq);' + LE +
		'      until nseq = 0;' + LE +
		'    end;' + LE +
		'  end;' + LE +
		'  result := Cp2Cpn_FlatCp1[Cpn2Cp_IFlatCp1[iv]];' + LE +
		'end;';

	writeln(src);
end.
*)

const
  Cp2Cpn_HashFlatStart: array[0 .. 15] of uint8 = (0,10,25,37,45,56,71,80,90,102,112,120,126,135,145,149);
  Cp2Cpn_HashFlatCount: array[0 .. 15] of uint8 = (10,15,12,8,11,15,9,10,12,10,8,6,9,10,4,5);
  Cp2Cpn_FlatCp1: array[0 .. 154] of TSystemCodePage =
  (
    $FFFF,720,864,1200,10000,12000,20000,20880,28592,51936,57008,737,865,1201,1361,10001,10017,10081,12001,20001,20273,20833,21025,28593,50225,57009,850,866,
    1026,1250,10002,10082,20002,20290,20866,28594,50930,57010,1251,10003,20003,21027,28595,50227,50931,57011,500,708,852,932,1140,1252,10004,20004,20420,20932,
    28596,37,437,869,949,1141,1253,10005,10021,20005,20261,20277,20949,28597,50229,50933,870,950,1142,1254,10006,20278,20838,28598,38598,775,855,1047,1143,
    1255,10007,20423,20871,28599,50935,856,936,1144,1256,10008,20280,20424,20936,28600,52936,54936,65000,857,1145,1257,20105,20297,20905,28601,29001,50937,
    65001,858,874,1146,1258,10010,20106,21866,57002,875,1147,20107,28603,50939,57003,860,1148,20108,20284,20924,28604,50220,51932,57004,861,1149,3021,10029,
    20269,20285,28605,50221,51949,57005,862,28606,50222,57006,863,10079,20127,28591,57007
  );
  Cp2Cpn_FlatNameStart: array[0 .. 153] of uint16 =
  (
    0,4,6,9,19,22,34,20,38,41,44,48,50,62,68,77,87,95,100,102,106,127,104,134,137,140,144,147,2,152,164,173,188,135,4,2,192,100,195,104,202,48,46,210,237,
    240,244,253,257,267,20,273,4,280,284,7,291,295,299,303,100,104,318,325,48,330,334,338,46,301,104,342,346,20,4,351,357,361,251,367,371,375,379,104,48,385,
    394,398,301,48,402,405,4,7,410,422,426,430,434,437,448,46,454,458,46,3,464,468,472,475,46,251,481,487,499,251,505,514,193,525,372,528,534,543,301,546,
    2,251,549,560,564,570,573,573,576,579,301,583,587,590,594,598,601,134,613,616,6,573,620,104,623,633,100,642
  );
  Cp2Cpn_FlatNames: array[0 .. 644] of ansichar =
    '720'#0'4'#0'16'#0'macintosh'#0'32'#0'Chinese_CNS'#0'880'#0'cn'#0'ka'#0'737'#0'5'#0'unicodefffe'#0'Johab'#0'japanese'#0'ukrainian'#0'turkish'#0'32BE'#0 +
    '1'#0'273'#0'BCDIC-KoreanExtended'#0'cp1025'#0'kr'#0'ma'#0'850'#0'66'#0'1026'#0'chinesetrad'#0'croatian'#0'x_Chinese-Eten'#0'290'#0'gu'#0'korean'#0'cp' +
    '21027'#0'ebcdic-japaneseanduscanada'#0'pa'#0'500'#0'asmo-708'#0'852'#0'shift_jis'#0'01140'#0'arabic'#0'420'#0'EUC-JP'#0'037'#0'437'#0'869'#0'ks_c_560' +
    '1-1987'#0'hebrew'#0'thai'#0'261'#0'277'#0'949'#0'870'#0'big5'#0'greek'#0'278'#0'-thai'#0'8-i'#0'775'#0'855'#0'01047'#0'cyrillic'#0'423'#0'871'#0'56'#0 +
    '2312'#0'chinesesimp'#0'280'#0'424'#0'936'#0'10'#0'hz-gb-2312'#0'18030'#0'857'#0'01145'#0'297'#0'905'#0'11'#0'uropa'#0'00858'#0'windows-874'#0'01146'#0 +
    'romanian'#0'ia5-german'#0'de'#0'01147'#0'-Swedish'#0'13'#0'be'#0'-Norwegian'#0'284'#0'00924'#0'14'#0'jp'#0'ta'#0'861'#0'MIK'#0'ce'#0'269'#0'285'#0'15' +
    #0'csISO2022JP'#0'te'#0'862'#0'as'#0'icelandic'#0'us-ascii'#0'or'#0;
  Cp2Cpn_PrefixStart: array[0 .. 153] of uint8 =
  (
    122,85,91,9,91,135,55,0,117,22,55,85,9,9,31,31,31,91,46,55,131,9,0,59,22,55,127,55,10,31,31,9,107,111,0,96,22,10,31,46,135,0,76,135,22,107,9,55,9,55,10,
    31,46,55,9,0,55,55,55,9,38,10,31,31,46,69,55,69,0,76,96,107,9,38,10,31,55,55,0,0,55,107,55,38,10,31,55,55,0,96,127,138,38,10,31,55,107,69,0,9,138,91,55,
    55,10,101,107,55,0,131,96,91,55,9,55,10,31,135,111,22,127,55,101,0,96,22,85,38,101,55,107,0,59,117,22,55,38,9,31,69,107,0,9,117,22,122,0,59,22,85,31,9,
    0,22
  );
  Cp2Cpn_Prefixes: array[0 .. 140] of ansichar =
    'iso-8859-'#0'windows-125'#0'x-iscii-'#0'x-mac-'#0'IBM0114'#0'x-cp2000'#0'ibm'#0'iso-2022-'#0'x-cp20'#0'x-cp5022'#0'IBM86'#0'utf-'#0'cp93'#0'x-IA5'#0'I' +
    'BM'#0'koi8-'#0'euc-'#0'DOS-'#0'cp8'#0'x-E'#0'x-'#0'gb'#0;
  Cpn2Cp_ToChilds: array[0 .. 836] of uint8 =
  (
    1,21,23,28,32,32,34,35,35,35,35,38,39,41,43,44,44,47,47,48,55,56,56,56,56,56,63,72,72,72,72,72,72,72,72,72,72,72,73,73,73,73,73,72,74,73,73,73,73,73,80,
    80,80,80,80,83,83,83,85,86,86,86,86,86,86,85,85,85,84,84,83,82,81,81,80,79,79,79,78,87,90,89,91,92,92,96,97,98,100,101,102,102,102,102,102,103,103,103,
    103,103,104,105,104,103,113,113,114,113,112,111,111,111,112,111,111,110,110,110,110,111,111,110,109,109,110,110,111,111,112,113,113,112,111,110,109,109,
    108,108,108,107,107,107,108,108,108,108,107,107,106,107,106,105,104,109,108,107,106,108,109,109,110,114,114,114,116,116,116,119,119,120,120,120,121,121,
    121,123,123,128,135,137,137,137,137,137,137,137,138,140,141,141,145,145,145,144,143,142,142,142,142,141,140,140,139,140,140,140,140,141,141,142,142,142,
    143,145,145,145,145,145,147,146,145,144,144,144,144,145,145,145,145,145,145,144,143,143,143,142,144,144,143,143,143,143,144,145,146,145,144,143,144,144,
    144,145,144,145,148,148,149,148,147,146,145,144,143,143,144,145,145,147,147,147,147,150,157,158,158,158,157,157,159,161,162,161,161,161,161,161,161,160,
    160,160,161,160,160,160,160,159,158,157,156,155,154,153,152,151,150,149,148,147,146,145,144,143,142,141,140,139,138,137,136,137,137,137,136,135,134,133,
    132,131,130,129,128,127,126,125,124,123,130,132,132,131,130,131,131,131,131,130,129,129,129,128,128,128,127,126,126,126,126,126,126,125,124,123,122,122,
    122,122,121,120,119,119,120,119,118,126,125,124,123,123,123,124,124,125,125,133,134,134,134,134,134,134,134,134,134,133,132,132,132,133,132,132,132,131,
    130,129,129,129,128,128,128,128,128,127,126,125,124,123,122,121,120,119,118,117,116,115,114,113,112,111,110,109,108,107,106,105,104,103,102,101,100,99,
    98,97,96,95,95,94,102,103,102,110,109,108,108,108,117,116,115,114,113,113,113,114,114,116,116,116,116,116,116,116,115,115,115,115,115,124,139,138,137,
    136,135,134,133,132,131,130,129,128,128,128,128,127,126,125,124,123,122,121,120,119,128,128,130,130,131,131,132,134,134,134,135,135,135,135,135,135,135,
    135,135,134,135,136,137,136,136,137,136,135,134,134,133,134,134,135,136,136,136,136,145,147,152,151,150,149,148,147,146,147,146,146,146,147,147,147,147,
    147,147,147,147,147,146,145,144,143,142,141,140,139,138,137,136,135,135,135,136,136,135,137,139,140,142,141,141,140,149,150,149,148,147,146,145,144,143,
    142,141,140,139,138,137,136,137,137,137,138,138,138,138,138,139,139,140,140,141,141,141,141,140,139,138,138,141,141,141,141,141,141,141,142,142,143,143,
    143,144,143,142,141,140,140,140,140,140,140,139,138,137,136,135,134,133,132,131,130,129,128,128,128,127,126,125,124,123,122,121,120,120,120,120,119,118,
    117,116,115,114,113,112,111,110,109,108,107,106,105,104,103,103,107,106,105,104,103,102,101,100,100,100,100,100,100,99,98,97,96,95,94,93,92,91,90,89,88,
    87,86,85,84,83,82,81,80,79,78,77,76,75,74,73,72,71,70,70,70,70,70,69,68,68,68,67,66,65,64,63,62,61,60,59,58,57,56,55,54,53,52,51,50,50,49,48,47,47,47,
    46,46,45,44,43,42,41,40,40,40,39,38,37,40,41,41,41,41,40,39,38,37,36,35,34,33,32,31,33,35,35,36,36,35,34,33,34,33,32,31,30,29,28,27,26,27,26,25,25,24,
    23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,5,5,5,5,4,3,2,1
  );
  Cpn2Cp_NChildsOrISeq: array[0 .. 836] of uint8 =
  (
    21,3,6,5,1,3,2,22,23,24,4,2,3,3,2,25,4,26,2,8,2,27,1,1,1,8,10,28,1,29,30,31,1,32,33,34,1,2,35,36,1,37,0,3,0,38,1,39,40,8,41,42,1,43,4,44,45,3,2,46,1,47,
    48,49,0,50,51,0,52,0,0,0,53,0,0,54,55,0,10,4,0,3,2,1,5,2,2,3,2,2,56,57,58,59,2,60,61,1,62,2,2,0,0,11,1,2,0,0,0,63,1,2,0,1,0,1,1,1,2,1,0,0,64,2,65,2,66,
    2,2,67,0,0,0,0,68,0,69,70,0,1,71,2,1,72,1,0,1,0,2,0,0,0,6,0,0,0,3,2,1,2,5,73,1,3,74,1,4,75,2,76,1,2,1,1,3,1,6,8,3,1,1,1,1,1,1,2,3,2,1,5,1,1,0,0,0,1,77,
    78,0,0,1,0,2,1,1,79,2,80,2,81,82,2,3,83,84,85,86,3,0,0,0,1,1,1,2,1,1,1,87,88,0,0,89,1,0,3,90,0,1,91,92,2,2,2,0,0,0,2,93,94,2,0,2,4,95,2,0,0,0,0,0,0,1,
    2,2,1,3,1,1,1,4,8,2,1,1,0,1,3,3,2,0,1,1,96,97,98,0,99,100,2,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    8,3,1,0,0,2,1,1,1,0,0,1,101,0,102,103,0,0,104,105,106,107,108,0,0,0,0,1,1,1,0,0,0,1,2,0,0,9,0,0,0,1,1,2,109,2,1,9,2,110,1,111,112,1,1,113,114,0,0,115,
    116,2,0,1,1,0,0,0,117,118,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,9,2,0,9,0,0,1,1,10,0,0,0,0,1,1,2,119,3,1,120,
    1,121,122,123,0,124,1,1,1,10,16,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,10,1,3,125,2,126,2,3,1,1,2,1,1,1,1,1,1,127,128,0,2,2,2,0,1,2,0,0,0,1,0,2,
    1,2,2,1,1,1,10,3,6,0,0,0,0,0,0,2,0,1,1,2,129,130,131,132,133,134,135,136,0,0,0,0,0,0,0,0,0,0,0,0,1,1,2,1,0,3,3,2,3,0,1,0,10,2,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,2,1,1,2,1,1,137,1,2,1,2,1,2,1,1,1,0,0,0,138,4,139,140,141,142,143,144,2,145,2,1,1,2,0,0,0,0,1,146,147,148,149,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,
    0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,5,0,0,0,0,0,0,0,150,1,151,1,152,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,153,154,
    155,156,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,157,158,0,159,0,0,0,0,0,0,1,160,0,0,0,4,2,1,1,1,0,0,0,0,0,0,0,0,0,0,3,3,1,2,1,0,0,0,2,0,0,
    0,0,0,0,0,0,2,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,161,162,163,164,0,0,0,0,0
  );
  Cpn2Cp_ParentCharsOrSeqCounts: array[0 .. 836] of ansichar =
    '-ice4adpwtsxmgkbujnlhvbsrpscnhybuclx3snroi'#23#7#6'hjwe-_siaberso'#3'tcns'#4'so1a234o59ez'#5'mov04581329ipswgkbaeu'#4'-'#5#6#5'c'#5#6'+7mc'#10#4's'#7 +
    '831'#3'i'#5#10'smxcaieu'#12#6'k'#7'2k_1'#4#3'_c-ri'#6'f'#3#5#6#7#7#3#5#4#8'04578132-9-6_830132056781020678903bs'#13#7#9#5'so'#3#4'u'#12'01gbcudnsfieg' +
    'jl-48'#5'i68-712_-s'#3'sa'#5'ph'#3'asbu'#7#3#7#4'8'#6'5_c'#3'8-f-1234593013203756781'#3'6789'#4'0i182'#5'68'#4'78917034002567801234569051092527380450' +
    '7503579mo8'#9#3'c891b'#3'py'#9'ek'#10#10'irtns'#10#10#6#11'jcki86784j5'#3#3#6'c125'#5'5'#4#5'rc218--_'#3#4'-5-r1378'#3'127890170340750257012345690109' +
    '7378045075r'#10#4#4#11#4'125246561058124t95l20pk524-'#6#9#9#9#9#9#20#9'pnr267012345678-201'#3'_--dbtaokmgpkj'#3'-'#7#7'08'#3#4#4#3'ru62'#7#7'5244-123' +
    '456789jk1234567898401234567893078'#3'7892'#3'0'#9#4#3#13'r840123456789cwunbyrdfsiegaht024jckahgruti5029'#3'79'#3'cegsneeaesraaua'#13#18'jc7878079ulbb' +
    '78788470123456789516013456 -pr:5'#5#5#5#5#5#5#5#5'700103780450703451234cagh59jkahtsloeu'#4'kiretssbrrer'#7'hyer'#5#5#5#4#7#8'hu'#8'010634n'#3#5#6#8'p' +
    'neee840123'#6'i'#3'-'#4#7#5#4#5'pr1'#5#6#6'a'#5'13451969s948019076878tsi'#3#3#3#3;
  Cpn2Cp_SeqCharStart: array[0 .. 142] of uint16 =
  (
    0,23,30,36,39,43,48,52,57,63,68,73,79,122,132,136,143,146,151,161,173,179,186,190,193,199,202,207,213,220,227,230,235,239,247,260,267,276,36,209,281,293,
    264,298,303,306,244,313,320,324,330,333,336,340,345,349,358,36,361,370,380,390,400,410,416,264,264,427,52,433,63,327,326,437,440,345,450,454,345,57,465,
    371,381,474,483,492,512,450,521,524,531,354,354,326,327,531,524,333,337,538,416,450,547,560,563,566,579,597,602,602,602,597,597,602,607,612,115,566,616,
    621,626,630,637,645,79,653,658,664,672,678,354,681,616,626,621,52,57,688,694,699,699,702,705
  );
  Cpn2Cp_SeqChars: array[0 .. 707] of ansichar =
    'c-multilingual-850+euroindows-is-620ig5ohabisualsid0ineserilliccdic-ma-11ot_928tended_unix_code_packed_format_for_japanesesi_x3.4-19abicn_66003iftedi' +
    'shn_850200_bchinese-eten_kanjicintoshrmaneek-hkscss-2icode-ascii_4551-1rwegiantingicalbrew-gb-2312c8codepage437hiftjisindows31jb2312nicode11utf7o-708' +
    '-big5nsinicode-2312-808030c_5601ean026thai46-us859-c56011987i8rs-37+euroo-277+euroe-278+euros-284+eurob-285+europ-kanaatin9--euro-cp932cii-ffe0646-uc' +
    's-2022-46.irv:1991-273+euro-297+euro-280+euroternational-500+euro-871+euroopa-0-utf--1-utf-8gb231280kdfmtjapanese027cnsoreanextendedapaneseanduscanad' +
    'a:1987:1988:1989oecerabicebrewreekomaniankrainiancelandicermanwedishorwegianvisualeucyrillicoatianrkishbitradimp';
  Cpn2Cp_IFlatCp1: array[0 .. 836] of uint8 =
  (
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,113,0,114,0,0,0,0,0,0,0,0,0,0,0,0,0,0,73,0,0,0,152,14,0,0,153,0,8,
    23,35,0,89,142,0,0,79,0,0,106,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,92,42,0,0,0,69,134,58,0,0,0,56,0,118,0,0,0,0,0,123,123,0,0,0,0,0,0,0,0,32,49,138,
    4,0,92,0,0,118,69,0,0,0,0,34,73,0,3,3,152,129,129,0,79,79,100,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,58,49,49,92,0,0,
    73,152,0,102,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,56,69,47,0,0,0,0,0,0,0,0,0,49,49,0,0,73,0,0,51,0,0,0,0,0,92,92,101,69,60,0,0,0,60,34,0,0,0,153,8,23,35,89,
    142,0,0,0,0,0,0,0,0,0,0,0,0,0,28,0,0,0,0,78,0,0,0,0,0,152,0,0,0,57,0,0,0,58,54,87,97,46,26,48,82,91,103,113,127,136,146,150,2,12,27,59,72,121,88,7,153,
    0,0,0,152,20,77,96,130,141,33,107,108,36,71,90,111,125,0,0,0,60,34,0,0,0,0,92,73,0,0,50,0,0,74,84,0,0,0,0,0,104,115,33,131,0,0,0,152,152,152,0,0,114,49,
    0,49,49,49,0,0,0,0,0,106,0,0,0,134,0,0,0,0,0,0,60,60,0,0,0,34,0,0,102,112,13,0,0,57,0,0,0,0,58,54,87,97,46,11,81,26,48,82,103,127,136,146,150,2,12,27,
    59,72,88,7,153,152,20,67,77,96,130,141,33,107,108,0,3,0,0,152,0,153,8,0,0,0,28,22,94,138,0,0,0,0,0,0,0,0,0,0,0,92,0,0,0,0,0,0,7,61,74,84,122,93,128,137,
    55,9,144,0,0,0,29,38,51,62,75,85,94,105,116,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,0,0,0,92,0,0,60,60,60,34,119,3,5,0,0,0,0,0,0,0,153,8,23,35,42,56,
    69,79,89,0,0,153,8,23,35,42,56,69,79,89,113,131,50,61,74,84,93,104,115,122,128,137,0,0,0,0,28,0,0,0,0,78,0,92,0,0,134,144,113,131,50,61,74,84,93,104,115,
    122,128,137,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,146,114,0,0,0,0,0,0,0,0,0,0,0,0,0,0,41,43,70,6,0,0,0,0,0,120,126,135,145,149,154,10,25,37,45,21,44,0,0,102,
    112,102,112,92,60,60,119,0,0,0,102,112,102,112,113,131,83,50,61,74,84,93,104,115,122,128,137,0,0,152,99,109,124,132,142,147,0,0,133,24,0,142,8,23,35,42,
    56,69,79,89,57,46,72,88,7,20,67,77,96,130,141,33,107,54,87,97,108,153,8,23,35,0,0,0,0,89,142,0,0,57,46,57,57,57,67,46,72,72,67,77,107,77,96,88,130,141,
    87,0,97,108,15,0,0,139,0,39,52,63,76,117,16,0,0,151,29,38,0,0,0,0,0,32,118,123,129,134,9,3,13,18,92,0,0,0,0,0,79,80,134,0,153,42,56,69,79,143,24,54,0,
    86,31,0,17,19,40,53,65,66,140,98,68,6,60,42,89,153,8,23,35,56,69,79,24,144,0,0,64,24,144,30,95
  );

function CodePageToCodePageName(cp: TSystemCodePage): AnsiString;
var
  fstart, i: SizeInt;
begin
  fstart := Cp2Cpn_HashFlatStart[cp and 15];
  i := IndexWord(PUint16(Cp2Cpn_FlatCp1)[1 + fstart], Cp2Cpn_HashFlatCount[cp and 15], cp);
  if i >= 0 then
    result := AnsiString(shortstring(PAnsiChar(@Cp2Cpn_Prefixes[Cp2Cpn_PrefixStart[fstart + i]])) + shortstring(PAnsiChar(@Cp2Cpn_FlatNames[Cp2Cpn_FlatNameStart[fstart + i]])))
  else
    result := '';
end;

function CodePageNameToCodePage(const cpname: AnsiString): TSystemCodePage;
var
  cp, ce, seqp: PAnsiChar;
  iv, iiv, ncis, nseq: SizeInt;
begin
  result := $FFFF;
  cp := PAnsiChar(pointer(cpname));
  ce := cp + length(cpname);
  iv := 0;
  while cp < ce do
  begin
    ncis := Cpn2Cp_NChildsOrISeq[iv];
    iv := iv + Cpn2Cp_ToChilds[iv];
    if ncis < 22 then
    begin
      iiv := IndexByte(Cpn2Cp_ParentCharsOrSeqCounts[iv], ncis, ord(LowerCase(cp^)));
      if iiv < 0 then exit;
      iv := iv + iiv;
      inc(cp);
    end else
    begin
      seqp := @Cpn2Cp_SeqChars[Cpn2Cp_SeqCharStart[ncis - 22]];
      nseq := ord(Cpn2Cp_ParentCharsOrSeqCounts[iv]);
      if nseq > ce - cp then exit;
      repeat
        if LowerCase(cp^) <> seqp^ then exit;
        inc(cp); inc(seqp); dec(nseq);
      until nseq = 0;
    end;
  end;
  result := Cp2Cpn_FlatCp1[Cpn2Cp_IFlatCp1[iv]];
end;

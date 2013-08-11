package icfpc2013

object StaticInput {
  val hex = List(
    "0000000000000000", "FFFFFFFFFFFFFFFF", "2A58520FB74CF334", "0A72EA451EAC43F3", "23D2B85622BBD9F0",
    "A234920B6A68E52B", "2F84E29EAEA83D34", "7CFC5FA16EF19AFC", "181309BF80F87ADC", "04AF6A254D210E5B",
    "41C851490795046F", "916C6A6ABB288059", "D03C4F0C1D53AA4C", "F33B78211D81AA73", "3A4B06DBE76FE622",
    "0C19F5328BADD029", "85AA5E95E618D8A5", "684F4B6FD9DBD5CC", "7E944E6505F4EE7E", "A29D606D57D8709C",
    "79AD51406A066082", "EDA9E3A93048C412", "6AC2D9B6D45B0326", "722BDAF092610079", "4275CEEF6BA51932",
    "143B1106494DD6C2", "C3A155EB73821DB5", "47D54A7C60A0D599", "078E6B0BF00FB938", "AF8764E3917D03B3",
    "2A090FDA1ABE5636", "7E66C42CA2424B69", "8C741103B95408CB", "D269B246A74B8B8A", "8544A8A57809BDAA",
    "2BF39508483C616C", "681F80C837D7E454", "A5541E533F990044", "9E64DA2E21A70886", "D746DA773B33A9B7",
    "1091DB4E416C0EF2", "5072C0D3E8A09BB0", "B2B94B9C687F0932", "F3BFAAC33A9A8774", "71B63F0E47F6D4F9",
    "5E51D4B7104EE5F2", "97DF75DAB9C2FA8B", "F059CD9F2F2ABD5F", "9F2A74855CE07E6E", "2DE9788E9835AC02",
    "2819981A459D9A61", "BCE5049E64924C4C", "D0C0AAB509AB918D", "4530FF108E98462B", "11DFBC4ACFFD13FA",
    "46ACE3B7FF6DA19F", "41CC43A2E962E544", "979BCF98594A2E8C", "D1DCCBA667BFBA8D", "941339C9CDE915D2",
    "BB8BB0D7F9C783B5", "1E21D6A1BC59E79A", "92C9B3263FCD93C8", "D4DF091242A3BAF2", "8B415FE0CE685CB8",
    "70B5864FABC2329C", "EAD8980A0CE8B24B", "F3420FB602AD7B23", "64CDB6D02B0DC856", "6B4966E9C49B962E",
    "F2A21C561A3E602B", "EFD6E03DF0BA8718", "3ADC20B80E22D94F", "4AC737589F8834BF", "FDC6B34EBE031970",
    "99198A1608E6149B", "65F52A33DF067F95", "189A9DABEB53F313", "51B60602F6A35697", "FD1CD20DB19E6D0B",
    "8C07ADE1476E7ACF", "ADB167936402EEC1", "934E67BEC0532710", "4A7DFAD95F944BD9", "9A3B10DF3A461CD7",
    "10135AE747C844B5", "48465A9622069BC1", "B3D91D908BC78D0A", "CB1C8F53CDB2DC3C", "7073961FE09F27AF",
    "B59223E1081D0181", "4F401231040B2BB8", "237998E8D928B6C1", "975D80DD168046C3", "983770B9C248EB9E",
    "D975E8198F9A9091", "08793D4ABBB863DC", "23DF4C5FC36D83D8", "B2410A7F617F0E58", "BAF0EE8EBA9A3056",
    "91C6A8AB4A015738", "97D4040EAFCDFA29", "E9C49B5541F5E963", "AAE54D385C9D7A5D", "02DEB03EA6719333",
    "D02F2DF3B2D454C7", "C42C82A2C4804269", "D2DA2CA3178C1710", "66D26EBEB1BD4B61", "31EBFB3E6FE97514",
    "5C37B99299CC6F86", "638B6EBD9D80766C", "A210F0E7D4B85707", "868208BD8F556949", "8D61C794095271CA",
    "2219F75F0579AE83", "6E09F1FC1FFB73A0", "3D01AF6B1DA0B2F1", "F3A4C6DA4CFADFBA", "92CC84BBD7FAA9FA",
    "9ABBA84020FF575C", "8739E4DDF267DD6C", "8AC778DD71C89FD0", "0CE317F2C5F0A68C", "A27DA17D5797939C",
    "9489ABD69C9B646D", "2CA96AD62D0F72F6", "313EED842D7B0385", "149AAF7FE9F74778", "FE3F14B65765EA7C",
    "EA3F2A44BD014F0F", "F3E105D0A96E6047", "9B8BE49B10A9A239", "53A5CE01F832BF75", "3F30896D3A9C896F",
    "A6813DA4E9AD0AC9", "6BE6A020B72835DF", "962744E59B8EB708", "6C4AF3D5BA44D15D", "38BFF6A9FBAEBE12",
    "B94FAAECF8F9199B", "601E55AF13DD4F37", "D939D2CB3790FA98", "30B44FF0FBD1B0C3", "670563DC767EDC56",
    "63408C4EC21C26DA", "3A3701D9901797CF", "B4D8EE6DE14EF38B", "89D2FC4DF7E44729", "E5BEF4DC8E2EC41E",
    "5AA55356C0446528", "5E995579B7EC1FB0", "797EC4A977CDEFE3", "E7807C13296CAB02", "D9C724D82C8082BA",
    "7301CF5023C4558D", "200588D67ABB9B87", "A317DFCEA36BC3D8", "DB0303BA00377F12", "8D53E5907CC28B0E",
    "ED34FCC18B4370CA", "30572F919E4111CA", "A768619A938331B2", "BD8169BE28A72A35", "557738DECE90FA99",
    "28A2DD041CB1B953", "826658DE5788658E", "9AD00DC31BB2FF99", "0AE1469FE7D1FA91", "4F665C6DFA0E56C2",
    "7FE6812A00CE538D", "D1278C6A915178E2", "2FD6BCFFCC7B0B85", "6B4C553D98B2A4BB", "FE3DD3EA861AED0E",
    "3DCC61E61F5C334E", "9AD46CFBBE8E13FD", "AF1C6287396372B5", "82A5A0D0119DF660", "C68914483FD1DA2F",
    "E3341E145D4C88B1", "4B60637DBB04EB7C", "1B2E924B3174F148", "B4880C0A4E1A556B", "56FB5ACD9FFF7F11",
    "04335BFE37632BB0", "155FC2146BD1D77F", "825FFC85BE3247FB", "2A074D3299BF8FC0", "F0DBE5050F072845",
    "CCF3DDA5DA4EA532", "BB14DC7E0557DE6C", "2727AB5018668914", "DD142D357B51E502", "92665AC09737B380",
    "BE0F6E954512ED61", "EBFAB3B68C14F199", "1E218953AB57FB02", "2D42BBE596D6E300", "FA4D59D44AB2600E",
    "409C39FB27F55A8B", "101D94CBC5376F35", "4A67E4DDFE099638", "794BD226DE98F6BF", "6C79C336D827825F",
    "DA12E22BE2E1D8E0", "3AB17F3426AB7C27", "606ED0DDAFF3554F", "81DDB2887BFD473A", "89CF3BA54694D819",
    "C2B96EB649A02ABC", "65E6DD2D62B161B1", "7A03858DCEF36978", "128D738C907ADC4C", "60A355F83585895C",
    "145764AC66DCF26B", "A3C720EF8E101E27", "3BD8C4DEB42E3AB9", "59242ACF90590B60", "1EBFA3D749A7CD16",
    "57412CD4B2917189", "184BC3B9CA1894F3", "1137DADAE199BA94", "F4681E8E21CAF4E0", "18E2816B0E157996",
    "2E0CDDDD9DE88F62", "B636FFF709B111BA", "3AD6A89692114909", "56E0A3632FA65877", "152A6B320897D7C2",
    "FA3EA4C6DAACA705", "D20AFE169E277369", "B40550733C540F0C", "F2384E3FB9BB7BA2", "B71C19333D92A69E",
    "8D85EC1D370B26E9", "63331BD351461F67", "DDD124B711188EE6", "9E2ED6E4AEDB1D23", "DADD0916F5B3AE05",
    "6F311AC1186EFF4F", "FBDD07E80EB72246", "B7F275CBE3D81223", "24DCFD5AF3CDC951", "EA8FA0FD744B5ABE",
    "BFBFDD4A2CED89CD", "5A22B09D1A1672A3", "E07C5BA013A4DDF1", "EBB80D4C8A572664", "9CD136F72D1DF35A",
    "67AB794CE901CB9C", "D72109825193741F", "E979A5F9FF228221", "2CBFC42C7D204D22", "21D71D986D54D47A",
    "6FFA3800EA9505A8")

  val hex0x = hex.map("0x" + _)

  val long = hex.map(HexString.toLong(_))
}

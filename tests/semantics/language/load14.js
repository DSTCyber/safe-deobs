var o1 = [];
var o2 = [123];
var o3 = [123,456];
var p;

if (@BoolTop)
	p = "0" 
else
	p = "1"

var __result1 = o1[p];
var __expect1 = undefined;

var __result2 = o2[p];
var __expect2 = 123;
var __result3 = o2[p];
var __expect3 = undefined;

var __result4 = o3[p]
var __expect4 = 123;
var __result5 = o3[p]
var __expect5 = 456;

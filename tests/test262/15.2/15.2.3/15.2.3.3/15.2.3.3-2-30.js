function testcase() 
{
  var obj = {
    "100000000000000000000" : 1
  };
  var desc = Object.getOwnPropertyDescriptor(obj, 100000000000000000000.123);
  return typeof desc !== "undefined" && desc.value === 1;
}
{
  var __result1 = testcase();
  var __expect1 = true;
}


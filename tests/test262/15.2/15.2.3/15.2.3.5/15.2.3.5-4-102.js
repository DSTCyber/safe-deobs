  function testcase() 
  {
    var proto = {
      configurable : true
    };
    var ConstructFun = (function () 
    {
      
    });
    ConstructFun.prototype = proto;
    var descObj = new ConstructFun();
    var newObj = Object.create({
      
    }, {
      prop : descObj
    });
    var result1 = newObj.hasOwnProperty("prop");
    delete newObj.prop;
    var result2 = newObj.hasOwnProperty("prop");
    return result1 === true && result2 === false;
  }
  {
    var __result1 = testcase();
    var __expect1 = true;
  }
  
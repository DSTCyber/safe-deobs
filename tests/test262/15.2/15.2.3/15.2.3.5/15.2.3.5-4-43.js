  function testcase() 
  {
    try
{      Object.create({
        
      }, {
        prop : false
      });
      return false;}
    catch (e)
{      return (e instanceof TypeError);}

  }
  {
    var __result1 = testcase();
    var __expect1 = true;
  }
  
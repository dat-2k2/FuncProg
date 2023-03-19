link = "test.json"
let data; 
// fetch(link).then((response)=>response.json()).then((json) => data=JSON.parse(json))
// console.log(data)
fetch("./test.json") // fetch the file 'test.json'
  .then(response => response.json()) // parse the response into JSON
  .then(jsonString => { // parse the JSON string into an array
    //const jsonArray = JSON.parse(jsonString);
    console.log(jsonString); // do something with the array, such as logging it to the console
  })
  .catch(err => { // handle any errors
    console.error(err);
  });
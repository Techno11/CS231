const fs = require('fs');

const fileData = fs.readFileSync("dictionary.txt", {encoding:'utf8', flag:'r'});

const splitData = fileData.split("\n");

const reduced = splitData.filter(s => s.length === 5);


console.log(reduced);

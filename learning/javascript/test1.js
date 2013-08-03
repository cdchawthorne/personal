/* 
Task:
----------------------------------------------------
Fill the array 'randomNumbers' with 64 random numbers. 
----------------------------------------------------

Requirements:
* You must use the provided getRandomNumber function.
* Make sure that there are no duplicate numbers in the array.
* Try to keep the running time reasonable!

*/

// You can't change this function
function getRandomNumber(callback) {
	var random = Math.floor( Math.random() * 70 );
	return setTimeout( function(){ callback(random); }, 1000);
}

var randomNumbers = [];

// Start your code below here
// -----------------------------------

/* 
 * numbersTaken[i] contains true iff
 * i is already in randomNumbers
 */
var numbersTaken = [];
for(var i = 0; i <= 69; ++i) {
    numbersTaken.push(false);
}

/*
 * A curried function of count and randomNum
 * If the randomNum is a new random number,
 * it pushes to randomNumbers and updates 
 * numbersTaken. Regardless, it then calls
 * getRandomNumber on assignRandomNumbers with 
 * the updated count.
 *
 * Must be curried so it can be partially
 * evaluated and passed to getRandomNumber.
 */
function assignRandomNumbers(count) {
    return function(randomNum) {
        if (count < 64) {
            if (! numbersTaken[randomNum]) {
                randomNumbers.push(randomNum);
                numbersTaken[randomNum] = true;
                getRandomNumber(assignRandomNumbers(count+1));
            } else {
                getRandomNumber(assignRandomNumbers(count));
            }
        }
    }
}

getRandomNumber(assignRandomNumbers(0));

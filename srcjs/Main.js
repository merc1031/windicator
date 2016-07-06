'use strict';
var util = require('util'),
    _ = require('underscore');

const ThrowTypes = {
  SINGLE_INNER: 'SINGLE_INNER',
  TRIPLE: 'TRIPLE',
  SINGLE_OUTER: 'SINGLE_OUTER',
  DOUBLE: 'DOUBLE',
  MISS: 'MISS'
};


module.exports = class Windicator {
  constructor(calculateThrowDataValue) {

    /* building a cache of values we can use to calculate the windicator */
    this.allPossibleValues = this.generate();
    this.throwValues = {};
    this.throwKeys = [];
    this.highestValue = 0;

    this.values = [];

    let throws = DartHelpersTest.listAllThrows();

    for (let i = 0, c = throws.length; i < c; i += 1) {
      let throwData = throws[i],
          value = calculateThrowDataValue(throwData);

      if (!this.throwValues[value]) {
        this.throwValues[value] = [];
        this.throwKeys.push(value);
      }
      this.throwValues[value].push(throwData);

    }
    this.throwKeys = this.throwKeys.sort((a, b) => b - a);

    this.highestValue = this.throwKeys[0];
  }

  /**
   * Takes in the goal number and and rounds remaining then returns the throws
   * needed to get to 0 (if possible)
   *
   * @param goal {number}
   * @param throwsRemaining {number}
   * @param throwHistory {Array}
   * @returns {Array{Array{}}
   */
  calculate(goal, throwsRemaining, throwHistory) {
    // make sure it's even possible to find a win
    if ((this.highestValue * throwsRemaining) >= goal) {
      let remaining = goal;
      let combinations = this.combsWithRepOuter(throwsRemaining, _.filter(this.allPossibleValues, (val) => val <= goal));
      this.values = this.dropBadValues(this.reWeigh(this.expandDarts(this.findCombinationsForTarget(remaining, combinations, []))));
      return this.values;
    }
    this.values = [];
    return this.values;
  }

  dropBadValues(choices) {
      return _.map(choices, (choice) => _.filter(choice, (val) => val.type != ThrowTypes.MISS));
  }

  getWeights(val, config) {
    let smear = (num) => {
        let offset = 0;
        // Bias toward cricket numbers
        if (num >= 15) {
            offset = (num - 1) / (20 - 1) / 10;
        }
        return offset + (((num - 1) / (20 - 1)) / 10);
    };
    // bias toward bigger numbers
    let singleChooser = () => val.number == 21 ? config.singleBull : (config.single + smear(val.number));
    let doubleChooser = () => val.number == 21 ? config.doubleBull : (config.double + smear(val.number));

    let weights = {};
    weights[ThrowTypes.SINGLE_OUTER] = singleChooser;
    weights[ThrowTypes.SINGLE_INNER] = singleChooser;
    weights[ThrowTypes.DOUBLE] = doubleChooser;
    weights[ThrowTypes.TRIPLE] = () => config.triple;
    weights[ThrowTypes.MISS] = () => config.miss;
    let w = weights[val.type]();
    return w;
  }

  static get perValueConfig() {
      return {
          miss: .9,
          triple: .5,
          double: .4,
          single: .9,
          doubleBull: .3,
          singleBull: .4
      };
  }

  static get perSetConfig() {
      return {
          miss: 0,
          triple: 0,
          double: 0,
          single: 0,
          doubleBull: 0,
          singleBull: 0
      };
  }
  /*
   * Assign weights to choice values, weigh each set of choices and return sorted by best choice
   */
  reWeigh(choices) {
    let smear = (num) => {
        let offset = 0;
        // Bias toward cricket numbers
        if (num >= 15) {
            offset = (num - 1) / (21 - 1) / 10;
        }
        return offset + (((num - 1) / (21 - 1)) / 10);
    };
    let perValueWeigher = (val) => {
      val.weight = this.getWeights(val, Windicator.perValueConfig);
      return val;
    };
    let perSetWeigher = (vals) => {
        let counts = {};
        vals.forEach((x) => {
            if (!counts[x.type]) counts[x.type] = {};
            if (!counts[x.type][x.number]) counts[x.type][x.number] = {};
            counts[x.type][x.number] = { count: (counts[x.type][x.number].count || 0) + 1, value: x };
        });
        let weights = _.reduce(counts, (outerAcc, valPerType, keyT) => {
            return outerAcc * _.reduce(valPerType, (acc, count, num) => {
                let weight = this.getWeights(count.value, Windicator.perSetConfig);

                return acc * (weight * count.count);
            }, 1);
        }, 1);
        return weights;
    };

    let choicesWithWeights = _.map(choices, (throws) => _.map(throws, perValueWeigher));
    return _.sortBy(choicesWithWeights, (throws) => {
        let pW = perSetWeigher(throws);
        let vW = _.reduce(throws, (acc, val) => acc * val.weight, 1);
        console.log(throws, pW, vW);
        return pW + vW;
    }).reverse();
  }

  /*
   * Takes any number of lists and makes all combinations
   * ex:
   *    cartesianProductOf([1,2],[4],[6,7,8])
   *    [[1,4,6],[1,4,7],[1,4,8],[2,4,6],[2,4,7],[2,4,8]]
   *
   */
  cartesianProductOf() {
    return _.reduce(arguments, function(a, b) {
        return _.flatten(_.map(a, function(x) {
            return _.map(b, function(y) {
                return x.concat([y]);
            });
        }), true);
    }, [ [] ]);
  }

  expandDarts(choices) {
    let findNumberForValue = (v) => v == 25 ? 21 : v;
    let generateDartsForValues = (vals) => {
        let generateDartForSingleValue = (val) => {
            // Special case for 0 -> MISS
            if (val == 0) {
                return [{type: ThrowTypes.MISS, value: 0, number: 0}];
            }
            // Build triple if the number is withing the valid range (any number evenly divisible by 3)
            let three = (val % 3) == 0 ? [Object.assign({}, {type: ThrowTypes.TRIPLE, value: val / 3, number: findNumberForValue(val / 3)})] : [];
            // Build double if the number is withing the valid range (any number evenly divisible by 2 as long as 40 and under or 50)
            let two = (val <= 40 || val == 50) && (val % 2) == 0 ? [Object.assign({}, {type: ThrowTypes.DOUBLE, value: val / 2, number: findNumberForValue(val / 2)})] : [];
            // Build single if the number is withing the valid range (any number 20 and under and 25)
            let one = val <= 20 || val == 25 ? [Object.assign({}, {type: ThrowTypes.SINGLE_OUTER, value: val, number: findNumberForValue(val)})] : [];

            // drop the empties
            let res = _.flatten([three, two, one], true);
            return res;
        };
        let res = _.map(vals, generateDartForSingleValue);
        // Generate all sensible combinations of the resulting lists
        return this.cartesianProductOf(...res);
    };
    let newChoices =  _.map(choices, generateDartsForValues);
    return _.flatten(newChoices, true);
  }

  /**
   * This is the algorithm to select from the available throw choices the ones that will get us to our desire target
   *
   * @param target {number}
   * @param choices {Array{Array{number}}}
   * @param acc {Array{Array{number}}}
   * @returns {Array{Array{number}}}
   */
  // Take a target, and a list of lists of available choices, The acc(umulator) will collect our results and return it as a list of lists of choices
  findCombinationsForTarget(target /* integer */, choices /* [[integers..]]*/, acc /* [[integers..]]*/) {
    if (choices.length == 0) {
      return acc; //Tail recursive return, return whatver has been accumulated
    }
    //Split the list into first element and rest of list
    var first = _.first(choices);
    var rest = _.rest(choices, 1);
    // Sum the first element and compare it to target
    var fun = (val) => target == _.reduce(val, (acc,r) => acc + r, 0);
    var res = fun(first) ? [first] : [];
    // Accumulate it into our list of results
    var res1 = res.concat(acc);
    // Tail call for optimization
    return this.findCombinationsForTarget(target, rest, res1);
  }

  /**
   * Algorithm for taking a list of available choices and returning all combinations (with repetitions) of the length we want
   *
   * @param count {number}
   * @param listOfAllChoices {Array{number}}
   * @returns {Array{Array{number}}}
   */
  // Find all combinations of listOfAllChoices with length of count. Returns a list of lists of choices
  combsWithRepOuter(count /* integer */, listOfAllChoices /* list of choices */) {
    let combinations = this.combsWithRep(count, listOfAllChoices);
    let resortedCombinations = _.map(combinations, (choices) => {
        let ord = _.sortBy(choices, (choice) => choice.value).reverse();
        return ord;
    });
    return resortedCombinations;
  }
  /**
   * Algorithm for taking a list of available choices and returning all combinations (with repetitions) of the length we want
   *
   * @param count {number}
   * @param listOfAllChoices {Array{number}}
   * @returns {Array{Array{number}}}
   */
  // Find all combinations of listOfAllChoices with length of count. Returns a list of lists of choices
  combsWithRep(count /* integer */, listOfAllChoices /* list of choices */) {
    if (count <= 0) {
      return [[]]; // Must return multidimensional empty of recursion fails to accumulate the outer value
    }
    if (listOfAllChoices.length == 0) {
      return [];
    }
    // Split the list into the first element and the rest of the list
    var first = _.first(listOfAllChoices);
    var rest = _.rest(listOfAllChoices, 1);
    // Recurse finding all smaller combinations available out of our total list of choices. Still have whole pool of choices available (allows for repetition)
    var leftIteration = this.combsWithRep((count - 1), listOfAllChoices);
    var newList = [];
    // For each sub combination (recursive), prepend the current scopes first element. Accumulate all results
    for (var i = 0; i < leftIteration.length; i++) {
        var elem = leftIteration[i];
        var innerList = [first].concat(elem);
        newList.push(innerList);
    }
  
    // Recurse finding all combinations using the remaining numbers in the list, but using the original count.
    var allTheRest = this.combsWithRep(count, rest);
    return newList.concat(allTheRest);
  
  }

  throwTypeOrdering(throwType) {
    let order = {};
    order[ThrowTypes.MISS] = 5;
    order[ThrowTypes.TRIPLE] = 4;
    order[ThrowTypes.DOUBLE] = 3;
    order[ThrowTypes.SINGLE_INNER] = 2;
    order[ThrowTypes.SINGLE_OUTER] = 1;
    return order[throwType];
  }

  generate() {
    return _.sortBy(_.uniq(_.flatten(Array.from(Array(21).keys(), (x) => [x,x*2,x*3]).concat([25,50]))));
  }
};

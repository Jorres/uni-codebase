'use strict';

function goodSort(arr, friends) {
    arr.sort((a, b) => {
        if (friends[a].name == friends[b].name) {
            return 0;
        }
        if (friends[a].name < friends[b].name) {
            return -1;
        }
        if (friends[a].name > friends[b].name) {
            return 1;
        }
    });
}

function Iterator(friends, filter, maxLevel = 1000000) {
    let curLayer   = [];
    let nameToId   = new Map();
    for (let i = 0; i < friends.length; i++) {
        if (friends[i].best) {
            curLayer.push(i);
        }
        nameToId.set(friends[i].name, i);
    }

    goodSort(curLayer, friends);

    let visited    = new Array(friends.length).fill(false);
    let suitable   = [];            
    let searchIteration = 0;
    while (curLayer.length > 0 && searchIteration < maxLevel) {
        let nextLayer = [];
        for (let i of curLayer) {
            let curFriend = friends[i];
            if (!visited[i]) {
                visited[i] = true;
                if (filter.check(curFriend.gender)) {
                    suitable.push(curFriend);
                }
                for (let neighbor of curFriend.friends) {
                    nextLayer.push(Number.parseInt(nameToId.get(neighbor)));
                }
            }            
        }
        searchIteration++;
        goodSort(nextLayer, friends);
        curLayer = nextLayer;
    }

    this.pos = 0;
    this.data = suitable;
    this.done = function() {
        return this.pos === this.data.length;
    };
    this.next = function() {
        if (this.done()) {
            return null;
        }
        return this.data[this.pos++];
    };
}
 
function LimitedIterator(friends, filter, maxLevel) {
    this.it = new Iterator(friends, filter, maxLevel);
    this.done = function() {
        return this.it.done();
    };
    this.next = function() {
        return this.it.next();
    };
}
 
function Filter() {
    this.check = function() {
        return true;
    };
}
 
function MaleFilter() {
    this.check = function(g) {
        return g == 'male';
    }
}
 
function FemaleFilter() {
    this.check = function(g) {
        return g == 'female';
    }
}

Object.setPrototypeOf(LimitedIterator.prototype, Iterator.prototype);
Object.setPrototypeOf(MaleFilter.prototype, Filter.prototype);
Object.setPrototypeOf(FemaleFilter.prototype, Filter.prototype);
 
exports.Iterator = Iterator;
exports.LimitedIterator = LimitedIterator;
exports.Filter = Filter;
exports.MaleFilter = MaleFilter;
exports.FemaleFilter = FemaleFilter;

const lib = require('./lib.js');
const assert = require('assert');

const friends = [
    { name: 'Sally',
        friends: ['Brad', 'Emily'],
        gender: 'female',
        best: true },
    { name: 'Julia',
        friends: ['Brad', 'Itan'],
        gender: 'female' },
    { name: 'Mat',
        friends: ['Sam', 'Sharon'],
        gender: 'male' },
    { name: 'Sharon',
        friends: ['Sam', 'Itan', 'Mat'],
        gender: 'female' },
    { name: 'Brad',
        friends: ['Sally', 'Emily', 'Julia'],
        gender: 'male' },
    { name: 'Sam',
        friends: ['Mat', 'Sharon'],
        gender: 'male',
        best: true },
    { name: 'Emily',
        friends: ['Sally', 'Brad'],
        gender: 'female' },
    { name: 'Itan',
        friends: ['Sharon', 'Julia'],
        gender: 'male' },
    {name: 'Isolated', 
        friends: [],
        gender: 'male'} ];

const shortMaleFriends = [
    { name: 'A',
        friends: ['B', 'C'],
        gender: 'male',
        best: true },
    { name: 'C',
        friends: ['B', 'A', 'D'],
        gender: 'male' },
    { name: 'D',
        friends: ['C', 'F'],
        gender: 'male' },
    { name: 'B',
        friends: ['A', 'C'],
        gender: 'male',
        best: true },
    { name: 'F',
        friends: ['D'],
        gender: 'female' }
];

const loopMaleFriends = [
    { name: 'A',
        friends: ['B', 'D', 'C'],
        gender: 'male',
        best: true },
    { name: 'C',
        friends: ['B', 'D', 'A'],
        gender: 'male'},
    { name: 'D',
        friends: ['A', 'C'],
        gender: 'male' },
    { name: 'B',
        friends: ['A', 'C'],
        gender: 'male',},
];

const shortFemaleFriends = [
    { name: 'B',
        friends: ['B', 'C'],
        gender: 'female',
        best: true },
    { name: 'C',
        friends: ['B', 'C', 'D'],
        gender: 'female',
        best: true },
    { name: 'A',
        friends: ['B', 'C'],
        gender: 'female',
        best: true },
    { name: 'D',
        friends: ['C'],
        gender: 'female' }
];

function friend(name, arr = friends) {
    let len = arr.length;

    while (len--) {
        if (arr[len].name === name) {
            return arr[len];
        }
    }
}

describe('lib.run', () => {
    beforeEach(() => {
    });
    it('Filter instanceof Filter', () => {
        assert.strictEqual((new lib.Filter()) instanceof lib.Filter, true);
    });
    it('MaleFilter instanceof Filter', () => {
        assert.strictEqual((new lib.MaleFilter()) instanceof lib.Filter, true);
    });
    it('FemaleFilter instanceof Filter', () => {
        assert.strictEqual((new lib.FemaleFilter()) instanceof lib.Filter, true);
    });
    it('MaleFilter instanceof MaleFilter', () => {
        assert.strictEqual((new lib.MaleFilter()) instanceof lib.MaleFilter, true);
    });
    it('FemaleFilter instanceof FemaleFilter', () => {
        assert.strictEqual((new lib.FemaleFilter()) instanceof lib.FemaleFilter, true);
    });
    it('Iterator instanceof Iterator', () => {
        let filter = new lib.Filter();
        assert.strictEqual((new lib.Iterator(friends, filter)) instanceof lib.Iterator, true);
    });
    it('LimitedIterator instanceof LimitedIterator', () => {
        let maleFilter = new lib.MaleFilter();
        assert.strictEqual((new lib.LimitedIterator(friends, maleFilter, 2)) instanceof lib.LimitedIterator, true);
    });
    it('LimitedIterator instanceof Iterator', () => {
        let maleFilter = new lib.MaleFilter();
        assert.strictEqual((new lib.LimitedIterator(friends, maleFilter, 2)) instanceof lib.Iterator, true);
    });
    it('Test from the task page', () => {
        const maleFilter = new lib.MaleFilter();
        const femaleFilter = new lib.FemaleFilter();
        const maleIterator = new lib.LimitedIterator(friends, maleFilter, 2);
        const femaleIterator = new lib.Iterator(friends, femaleFilter);

        const invitedFriends = [];

        while (!maleIterator.done() && !femaleIterator.done()) {
            invitedFriends.push([
                maleIterator.next(),
                femaleIterator.next()
            ]);
        }

        while (!femaleIterator.done()) {
            invitedFriends.push(femaleIterator.next());
        }

        assert.deepStrictEqual(invitedFriends, [ [friend('Sam'), friend('Sally')],
                                                 [friend('Brad'), friend('Emily')],
                                                 [friend('Mat'), friend('Sharon')],
                                                 friend('Julia') ])
    }),
    it('Depth one', () => {
        const maleFilter = new lib.MaleFilter();
        const femaleFilter = new lib.FemaleFilter();
        const maleIterator = new lib.LimitedIterator(friends, maleFilter, 1);
        const femaleIterator = new lib.LimitedIterator(friends, femaleFilter, 1);

        const invitedFriends = [];

        while (!maleIterator.done() && !femaleIterator.done()) {
            invitedFriends.push([
                maleIterator.next(),
                femaleIterator.next()
            ]);
        }

        assert.deepStrictEqual(invitedFriends, [ [friend('Sam'), friend('Sally')] ]);
    }),
    it('Depth unlimited', () => {
        const maleFilter = new lib.MaleFilter();
        const femaleFilter = new lib.FemaleFilter();
        const maleIterator = new lib.Iterator(friends, maleFilter);
        const femaleIterator = new lib.Iterator(friends, femaleFilter);

        const invitedFriends = [];

        while (!maleIterator.done() && !femaleIterator.done()) {
            invitedFriends.push([
                maleIterator.next(),
                femaleIterator.next()
            ]);
        }

        assert.deepStrictEqual(invitedFriends, [ [friend('Sam'), friend('Sally')],
                                                 [friend('Brad'), friend('Emily')],
                                                 [friend('Mat'), friend('Sharon')],
                                                 [friend('Itan'), friend('Julia')]])
    })

    it('Males only depth unlim', () => {
        const maleFilter = new lib.MaleFilter();
        const maleIterator = new lib.Iterator(loopMaleFriends, maleFilter);

        const invitedFriends = [];
        while (!maleIterator.done()) {
            invitedFriends.push(maleIterator.next());
        }

        console.log(invitedFriends);
        
        assert.deepStrictEqual(invitedFriends, [
            friend('A', loopMaleFriends),
            friend('B', loopMaleFriends),
            friend('C', loopMaleFriends),
            friend('D', loopMaleFriends),
        ]);
    })

    it('Isolated friend never picked up', () => {
        const maleFilter = new lib.MaleFilter();
        const maleIterator = new lib.Iterator(friends, maleFilter);

        const invitedFriends = [];
        while (!maleIterator.done()) {
            invitedFriends.push(maleIterator.next());
        }

        assert.deepStrictEqual(invitedFriends, [
                 friend('Sam'), 
                 friend('Brad'),   
                 friend('Mat'),    
                 friend('Itan')
        ]);
    });
    it('Males only depth 1', () => {
        const maleFilter = new lib.MaleFilter();
        const maleIterator = new lib.LimitedIterator(shortMaleFriends, maleFilter, 1);

        const invitedFriends = [];
        while (!maleIterator.done()) {
            invitedFriends.push(maleIterator.next());
        }

        assert.deepStrictEqual(invitedFriends, [
            friend('A', shortMaleFriends),
            friend('B', shortMaleFriends),
        ]);
    })

    it('Males only default filter', () => {
        const filter = new lib.Filter();
        const maleIterator = new lib.LimitedIterator(shortMaleFriends, filter, 1);

        const invitedFriends = [];
        while (!maleIterator.done()) {
            invitedFriends.push(maleIterator.next());
        }

        assert.deepStrictEqual(invitedFriends, [
            friend('A', shortMaleFriends),
            friend('B', shortMaleFriends),
        ]);
    })

    it('Females only depth unlim', () => {
        const femaleFilter = new lib.FemaleFilter();
        const femaleIterator = new lib.Iterator(shortFemaleFriends, femaleFilter);

        const invitedFriends = [];
        while (!femaleIterator.done()) {
            invitedFriends.push(femaleIterator.next());
        }

        console.log(invitedFriends);

        assert.deepStrictEqual(invitedFriends, [
            friend('A', shortFemaleFriends),
            friend('B', shortFemaleFriends),
            friend('C', shortFemaleFriends),
            friend('D', shortFemaleFriends)
        ]);
    })

    it('Females only depth 1', () => {
        const femaleFilter = new lib.FemaleFilter();
        const femaleIterator = new lib.LimitedIterator(shortFemaleFriends, femaleFilter, 1);

        const invitedFriends = [];
        while (!femaleIterator.done()) {
            invitedFriends.push(femaleIterator.next());
        }

        assert.deepStrictEqual(invitedFriends, [
            friend('A', shortFemaleFriends),
            friend('B', shortFemaleFriends),
            friend('C', shortFemaleFriends)
        ]);
    })

    it('Females only default filter', () => {
        const filter = new lib.Filter();
        const femaleIterator = new lib.LimitedIterator(shortFemaleFriends, filter, 1);

        const invitedFriends = [];
        while (!femaleIterator.done()) {
            invitedFriends.push(femaleIterator.next());
        }

        assert.deepStrictEqual(invitedFriends, [
            friend('A', shortFemaleFriends),
            friend('B', shortFemaleFriends),
            friend('C', shortFemaleFriends)
        ]);
    })
});

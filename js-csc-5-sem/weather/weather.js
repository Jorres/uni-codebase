'use strict';

const fetch = require('node-fetch');

const API_KEY = require('./key.json');

class TripBuilder {
    constructor(geoids) {
        this.geoids = geoids;
        this.wanted = [];
        this.maxDays = 7;
        this.totalDays = 0;
    }

    sunny(daysCount) {
        this.totalDays += daysCount;
        for (let i = 0; i < daysCount; i++) {
            this.wanted.push("sunny");
        }
        return this;
    }

    cloudy(daysCount) {
        this.totalDays += daysCount;
        for (let i = 0; i < daysCount; i++) {
            this.wanted.push("cloudy");
        }
        return this;
    }

    max(daysCount) {
        this.maxDays = daysCount;
        return this;
    }

    form(geoid) {
        return 'https://api.weather.yandex.ru/v2/forecast?hours=false&geoid=' 
               + geoid + '&limit=7';
    }

    suitable(actual, needed) {
        if (needed === 'sunny') {
            return actual === 'clear' || actual === 'partly-cloudy';
        } 
        if (needed === 'cloudy') {
            return actual === 'cloudy' || actual === 'overcast';
        }
        console.assert(false);
    }

    dfs(visited, conditions, curday, trail, v) {
        visited[v] = true;
        let left = this.maxDays;
        while (left > 0 && curday < this.totalDays && this.suitable(conditions[v][curday], this.wanted[curday])) {
            trail.push({geoid: this.geoids[v], day: curday + 1});
            curday++;
            left--;
        }
        if (curday === this.totalDays) {
            return {
                success: true,
                result: trail
            };
        }

        if (left === this.maxDays) {
            visited[v] = false;
            return {success: false};
        }

        for (let i = 0; i < this.geoids.length; i++) {
            if (visited[i]) {
                continue;
            }
            let res = this.dfs(visited, conditions, curday, trail, i);
            if (res.success) {
                return res;
            }
        }
        while (left < this.maxDays) {
            trail.pop();
            left++;
        }
        visited[v] = false;
        return {success: false};
    }

    makeRejected() {
        return Promise.reject(Error("Не могу построить маршрут!"));
    }

    async build() {
        if (this.totalDays > 7) {
            return this.makeRejected();
        }

        let townData = await Promise.all(
            this.geoids.map(
                geoid => fetch(this.form(geoid))
                            .then(res => res.json())));

        let visited = new Array(this.geoids.length).fill(false);
        let conditions = [];
        console.assert(townData[0] == townData[1]);
        townData.forEach(town => {
            let curTownConds = town.forecasts.map(forecast => forecast.parts.day_short.condition);
            conditions.push(curTownConds);
        });

        for (let i = 0; i < this.geoids.length; i++) {
            let traversal = this.dfs(visited, conditions, 0, [], i);
            if (traversal.success) {
                return Promise.resolve(traversal.result);
            }
        }
        return this.makeRejected();
    }
}

/**
 * Фабрика для получения планировщика маршрута.
 * Принимает на вход список идентификаторов городов, а
 * возвращает планировщик маршрута по данным городам.
 *
 * @param {number[]} geoids Список идентификаторов городов
 * @returns {TripBuilder} Объект планировщика маршрута
 * @see https://yandex.ru/dev/xml/doc/dg/reference/regions-docpage/
 */
function planTrip(geoids) {
    return new TripBuilder(geoids);
}

module.exports = {
    planTrip
};

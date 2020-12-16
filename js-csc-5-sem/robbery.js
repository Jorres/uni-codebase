'use strict';
 
/**
 * @param {Object} schedule Расписание Банды
 * @param {number} duration Время на ограбление в минутах
 * @param {Object} workingHours Время работы банка
 * @param {string} workingHours.from Время открытия, например, "10:00+5"
 * @param {string} workingHours.to Время закрытия, например, "18:00+5"
 * @returns {Object}
 */
function getAppropriateMoment(schedule, duration, workingHours) {
  return {
    parseTimezone(time) {
      return Number.parseInt(time.substring(time.indexOf('+')));
    },

    convertToMs(str) {
      let timezone = this.parseTimezone(str);
      let dayToNum = {
        'ПН': 1,
        'ВТ': 2,
        'СР': 3
      };

      let ans = Date.UTC(2020, 
                         0, 
                         dayToNum[str.substring(0, 2)], 
                         Number.parseInt(str.substring(3, 5)) - timezone,
                         Number.parseInt(str.substring(6, 8)));
      return ans;
    },

    pushBankDataAsRobber(ans, workingHours) {
      for (let s of ['ПН ', 'ВТ ', 'СР ']) {
        let timeFrom = s + workingHours.from;
        ans.push({
          date: this.convertToMs(timeFrom),
          sign: "+",
          name: 'bank',
          day: timeFrom.substring(0, 2)
        })

        ans.push({
          date: this.convertToMs(s + workingHours.to),
          sign: "-",
          name: 'bank',
          day: timeFrom.substring(0, 2)
        })
      }
    },
    
    parseDatas(schedule, workingHours) {
      let ans = [];

      for (let robber in schedule) {
        let apprTimes = schedule[robber];
        apprTimes.forEach(segment => {
          let from = this.convertToMs(segment.from);
          let to = this.convertToMs(segment.to);
          
          ans.push({
            "date": from,
            "sign": '-',
            "name": robber,
            "day": segment.from.substring(0, 2)
          })

          ans.push({
            "date": to,
            "sign": '+',
            "name": robber,
            "day": segment.to.substring(0, 2)
          })
        })
      }

      this.pushBankDataAsRobber(ans, workingHours);
      return ans;
    },

    debugData(data, timezone) {
      let date = new Date(data.date + timezone * 60 * 60 * 1000);
      return date.toUTCString();
    },

    exists() {
      // this.cachedSchedule = schedule;
      // this.cachedDuration = duration;
      // this.cachedWorkingHours = cachedWorkingHours;
      let scheduleDatas = this.parseDatas(schedule, workingHours);
      let sortedDatas = scheduleDatas.sort((d1, d2) => {
        return d1.date - d2.date;
      })
      
      let totalFree = 3; // everybody is free by default except for a bank
      for (let i = 0; i < sortedDatas.length - 1; i++) {
        if (sortedDatas[i].sign == '+') {
          totalFree++;
        } else {
          totalFree--;
        }

        if (totalFree == 4) { // 3 robbers + bank
          if (sortedDatas[i + 1].date - sortedDatas[i].date >= duration * 60 * 1000) {
            this.bankZone = this.parseTimezone(workingHours.from);
            this.msIn0 = sortedDatas[i].date;
            this.day = sortedDatas[i].day;
            return true;
          }
        }
      }
      return false;
    },
 
    format(template) {
      if (!this.exists()) {
        return "";
      }

      let timestamp = this.msIn0 + this.bankZone * 60 * 60 * 1000;
      let date = new Date(timestamp);
      let hours = date.getUTCHours();
      if (Number.parseInt(hours) < 10) {
        hours = "0" + hours;
      }
      let minutes = date.getMinutes();
      if (Number.parseInt(minutes) < 10) {
        minutes = "0" + minutes;
      }

      return template.replace(/%HH/, hours)
                     .replace(/%MM/, minutes)
                     .replace(/%DD/, this.day);
    },
 
    tryLater() {
      return false;
    },

    msIn0: 0,
    bankZone: 0,
    day: ""
  };
}

let isExtraTaskSolved = true;

module.exports = {
  getAppropriateMoment,
  isExtraTaskSolved
};




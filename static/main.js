toastr.options.escapeHtml = true; // to prevent XSS vulnerabilities

if(!Push.Permission.has()) {
  alert("タブが表示されていない時でも友人からの通知を受け取るために通知を許可してください");
  Push.Permission.request();
}


const ws_uri = (function(){
  const loc = window.location;
  let new_uri;
  if (loc.protocol === "https:") new_uri = "wss:";
  else new_uri = "ws:";
  new_uri += "//" + loc.host;
  new_uri += loc.pathname + "ws";
  return new_uri;
})();
const ws = new WebSocket(ws_uri);



const baseImage = new Image();
baseImage.src = "/assets/images/base.png";

const baseLayer = genBaseLayer();
const objectLayer = genObjectLayer();
const seatMap = genSeatMap();

const getSeat = function(x, y) {
  let result;
  Object.keys(seatMap).forEach(function(table){
    Object.keys(seatMap[table]).forEach(function(seat){
      if(seatMap[table][seat][0] == y && seatMap[table][seat][1] == x)
        result = [Number(table), Number(seat)];
    });
  });
  return result;
}



const app = new Vue({
  el: '#app',
  data: {
    ctx: undefined,

    userName: "",
    userImage: "",

    table: undefined,
    tableMessage: "Remote Houseへようこそ",

    userStore: {},
    seatingChartStore: {},
    tableMessageStore: {},
  },
  mounted: function(){
    const canvas = document.getElementById('house-map');
    this.ctx = canvas.getContext('2d');
    this.ctx.textAlign = "center";
    this.ctx.fillStyle = "white";
  },
  methods: {
    genRandomUserImage: function() {
      const n = Math.floor(Math.random() * 17);
      this.userImage = "https://placekitten.com/64/64?image=" + n;
      this.updateUserInfo();
    },
    updateUserInfo: function() {
      if(this.userName.length == 0 || this.userImage.length == 0) return;
      ws.send(makeUpdateUserInfo(this.userName, this.userImage));
    },
    clickCanvas: function(e) {
      const x = Math.floor(e.offsetX / 32);
      const y = Math.floor(e.offsetY / 32);
      const tableAndSeat = getSeat(x, y);
      if(tableAndSeat) ws.send(makeActOnSeat(tableAndSeat[0], tableAndSeat[1]));
    },
    notifyPoke: function(fromUserId) {
      const user = this.userStore[fromUserId];
      if(!user) return;
      const userName = user["userName"];
      const message = userName + "からPokeが来ました"
      toastr.info(message);
      Push.create(message, {
        onClick: function () {
          window.focus();
          this.close();
        }
      })
    },
    pokeSended: function(toUserId) {
      const user = this.userStore[toUserId];
      if(!user) return;
      const userName = user["userName"];
      const message = userName + "にPokeを送りました"
      toastr.info(message);
    },
    moveToSeat: function(table, seat) {
      this.table = table;
      this.tableMessage = this.tableMessageStore[table];
    },
    updateTableMessage: function() {
      if(this.table < 0) return;
      ws.send(makeUpdateTableMessage(this.table, this.tableMessage));
    },
    refreshTableMessage: function() {
      if(this.table < 0) return;
      this.tableMessage = this.tableMessageStore[this.table];
    },
    drawBaseImage: function(srcX, srcY, destX, destY) {
      const s = 16;
      this.ctx.drawImage(baseImage, s * srcX, s * srcY, s, s, s * destX, s * destY, s, s);
    },
    getUserImageWithCache: (function(){
      const userImageCache = {};
      return function(userImage, cb) {
        const cachedImage = userImageCache[userImage];
        if(cachedImage) cb(cachedImage)
        else {
          const imageObj = new Image();
          imageObj.onload = function() {
            cb(imageObj);
          }
          imageObj.src = userImage;
          userImageCache[userImage] = imageObj;
        }
      }
    })(),
    updateHouseMap: function() {
      for(let y = 0; y < baseLayer.length; y++)
        for(let x = 0; x < baseLayer[0].length; x++)
          this.drawBaseImage(baseLayer[y][x][0], baseLayer[y][x][1], x, y);
      for(let y = 0; y < objectLayer.length; y++)
        for(let x = 0; x < objectLayer[0].length; x++)
          if(objectLayer[y][x].length > 0)
            this.drawBaseImage(objectLayer[y][x][0], objectLayer[y][x][1], x, y);

      const $this = this;
      Object.keys(this.seatingChartStore).forEach(function(userId){
        const user = $this.userStore[userId];
        if(!user) return;
        const userName = user["userName"];
        const userImage = user["userImage"];
        const table = $this.seatingChartStore[userId]["table"];
        const seat = $this.seatingChartStore[userId]["seat"];
        const s = 16;
        const y = s * seatMap[table][seat][0];
        const x = s * seatMap[table][seat][1];

        $this.ctx.fillText(userName, x + s/2, y - s/3);
        $this.getUserImageWithCache(userImage, function(imageObj) {
          $this.ctx.drawImage(imageObj, x, y, s, s);
        });
      });
    }
  }
});

ws.onopen = function(e) {
  ws.send(makeRequestHouseInfo());
};

ws.onmessage = function (e) {
  data = JSON.parse(e.data);
  if (data["type"] == "updateUserInfoSucceed")
    toastr.success("ユーザー情報が更新されました");
  else if (data["type"] == "userNotRegistered")
    toastr.error("ユーザー情報が登録されていません");
  else if (data["type"] == "moveToSeat")
    app.moveToSeat(data["value"]["table"], data["value"]["seat"]);
  else if (data["type"] == "notifyHouseInfo") {
    app.userStore = data["value"]["userStore"];
    app.seatingChartStore = data["value"]["seatingChartStore"];
    app.tableMessageStore = data["value"]["tableMessageStore"];
    app.updateHouseMap();
  }
  else if (data["type"] == "notifyUserStore") {
    app.userStore = data["value"];
    app.updateHouseMap();
  }
  else if (data["type"] == "notifySeatingChartStore") {
    app.seatingChartStore = data["value"];
    app.updateHouseMap();
  }
  else if (data["type"] == "notifyTableMessageStore")
    app.tableMessageStore = data["value"];
  else if (data["type"] == "notifyPoke")
    app.notifyPoke(data["value"]["from"]);
  else if (data["type"] == "pokeSended")
    app.pokeSended(data["value"]["to"]);
};

function makeRequestHouseInfo() {
  return JSON.stringify({"type": "requestHouseInfo"});
}

function makeUpdateUserInfo(userName, userImage) {
  return JSON.stringify({
    "type": "updateUserInfo",
    "value": {"userName": userName, "userImage": userImage}
  });
}

function makeActOnSeat(table, seat) {
  return JSON.stringify({
    "type": "actOnSeat",
    "value": {"table": table, "seat": seat}
  });
}

function makeUpdateTableMessage(table, tableMessage) {
  return JSON.stringify({
    "type": "updateTableMessage",
    "value": {"table": table, "tableMessage": tableMessage}
  });
}



function genBaseLayer() {
  const g = [0, 4];
  const o = [0, 59];
  const t = [0, 60];
  const l = [0, 61];
  const b = [0, 62];
  const a = [1, 60];
  const c = [1, 61];
  const d = [1, 62];
  const i = [2, 59];
  const e = [2, 60];
  const f = [2, 61];
  const h = [2, 62];
  const j = [2, 49];
  return [
      [g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g]
    , [g, o, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, i, g]
    , [g, o, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, i, g]
    , [g, o, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, i, g]
    , [g, o, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, i, g]
    , [g, o, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, i, g]
    , [g, o, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, i, g]
    , [g, o, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, i, g]
    , [g, o, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, i, g]
    , [g, o, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, i, g]
    , [g, o, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, i, g]
    , [g, o, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, i, g]
    , [g, o, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, i, g]
    , [g, o, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, i, g]
    , [g, o, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, i, g]
    , [g, o, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, i, g]
    , [g, o, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, i, g]
    , [g, o, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, i, g]
    , [g, o, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, i, g]
    , [g, o, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, j, i, g]
    , [g, t, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, e, g]
    , [g, l, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, c, f, g]
    , [g, b, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, h, g]
    , [g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g]
    ];
}

function genObjectLayer() {
  const o = [];
  const a = [4, 104];
  const b = [5, 104];
  const c = [5, 105];
  const d = [5, 106];
  const e = [6, 104];
  const f = [6, 105];
  const g = [6, 106];
  const h = [7, 104];
  const i = [7, 105];
  const j = [7, 106];
  const k = [7, 129];
  const l = [7, 130];
  const m = [0, 98];
  const n = [0, 99];
  const p = [1, 100];
  const q = [2, 98];
  const s = [1, 123];
  const t = [0, 123];
  const u = [3, 125];
  const v = [5, 125];
  const r = [3, 131];
  const w = [3, 132];
  return [
      [o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o]
    , [o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o]
    , [o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o]
    , [o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o]
    , [o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, k, o, o, m, s, t, t, u, u, u, v, v, s, o, o]
    , [o, o, o, o, o, a, o, a, o, a, o, a, o, a, o, o, o, l, o, o, n, a, o, o, o, o, o, o, o, o, o, o]
    , [o, o, o, o, b, e, e, e, e, e, e, e, e, e, h, o, o, o, o, a, n, o, o, a, o, o, o, a, o, o, o, o]
    , [o, o, o, a, c, f, f, f, f, f, f, f, f, f, i, a, o, k, o, o, p, q, q, q, q, q, q, q, q, q, o, o]
    , [o, o, o, o, c, f, f, f, f, f, f, f, f, f, i, o, o, l, o, o, a, o, a, o, a, o, a, o, a, o, o, o]
    , [o, o, o, a, c, f, f, f, f, f, f, f, f, f, i, a, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o]
    , [o, o, o, o, d, g, g, g, g, g, g, g, g, g, j, o, o, k, o, o, o, o, o, o, o, o, o, o, o, o, o, o]
    , [o, o, o, o, o, a, o, a, o, a, o, a, o, a, o, o, o, l, o, o, a, b, h, o, o, a, b, h, o, o, o, o]
    , [o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, d, j, a, o, o, d, j, a, o, o, o]
    , [o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o]
    , [o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, k, o, o, o, o, o, o, o, o, o, o, o, o, o, o]
    , [o, o, o, o, a, o, a, o, a, o, a, o, a, o, a, o, o, l, o, o, o, o, a, o, r, o, a, o, o, o, o, o]
    , [o, o, o, o, b, e, e, e, h, o, b, e, e, e, h, o, o, o, o, o, o, o, o, o, w, o, o, o, o, o, o, o]
    , [o, o, o, o, d, g, g, g, j, o, d, g, g, g, j, o, o, k, o, o, o, o, a, o, o, o, a, o, o, o, o, o]
    , [o, o, o, o, a, o, a, o, a, o, a, o, a, o, a, o, o, l, o, o, o, o, o, o, o, o, o, o, o, o, o, o]
    , [o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o]
    , [o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o]
    , [o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o]
    , [o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o]
    , [o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o]
    ];
}

function genSeatMap() {
  return {
    0: {
      0:  [5,  5],
      1:  [5,  7],
      2:  [5,  9],
      3:  [5,  11],
      4:  [5,  13],
      5:  [7,  3],
      6:  [7,  15],
      7:  [9,  3],
      8:  [9,  15],
      9:  [11, 5],
      10: [11, 7],
      11: [11, 9],
      12: [11, 11],
      13: [11, 13],
    },
    1: {
      0: [15, 4],
      1: [15, 6],
      2: [15, 8],
      3: [18, 4],
      4: [18, 6],
      5: [18, 8],
    },
    2: {
      0: [15, 10],
      1: [15, 12],
      2: [15, 14],
      3: [18, 10],
      4: [18, 12],
      5: [18, 14],
    },
    3: {
      0: [5, 21],
      1: [6, 19],
      2: [6, 23],
      3: [6, 27],
      4: [8, 20],
      5: [8, 22],
      6: [8, 24],
      7: [8, 26],
      8: [8, 28],
    },
    4: {
      0: [11, 20],
      1: [12, 23],
    },
    5: {
      0: [11, 25],
      1: [12, 28],
    },
    6: {
      0: [15, 22],
    },
    7: {
      0: [15, 26],
    },
    8: {
      0: [17, 22],
    },
    9: {
      0: [17, 26],
    },
  }
}

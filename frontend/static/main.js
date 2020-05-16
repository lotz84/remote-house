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

const baseLayer = PS.Main.baseLayer;
const objectLayer = PS.Main.objectLayer;
const seatMap = PS.Main.seatMap;

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

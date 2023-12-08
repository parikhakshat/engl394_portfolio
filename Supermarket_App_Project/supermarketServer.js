const express = require("express");
const fs = require("fs");

const app = express();
const portNumber = 5000;

class Item {
  #price;
  constructor(name, price) {
    this.name = name;
    this.#price = price;
  }

  getPrice() {
    return this.#price;
  }
}

if (process.argv.length != 3) {
  console.log("Usage supermarketServer.js jsonFile");
} else {
  app.set("views", "templates");
  app.set("view engine", "ejs");
  app.use(express.urlencoded({ extended: true }));

  app.get("/", (request, response) => {
    response.render("index");
  });

  app.get("/catalog", (request, response) => {
    let list = itemsList.map((curr) => new Item(curr.name, curr.cost));

    let itemsTable = "<table border='1'><tr><th>Name</th><th>Cost</th></tr>";

    list.forEach((curr) => {
      num = Number(curr.getPrice());
      if (!isNaN(num)) {
        itemsTable +=
          "<tr><td>" + curr.name + "</td><td>" + num.toFixed(2) + "</td></tr>";
      }
    });

    itemsTable += "</table>";

    response.render("displayItems", { itemsTable: itemsTable });
  });

  app.get("/order", (request, response) => {
    let list = itemsList.map((curr) => new Item(curr.name, curr.cost));

    let items = "";

    list.forEach((curr) => {
      items +=
        '<option value="' +
        curr.name +
        "," +
        curr.getPrice().toFixed(2) +
        '">' +
        curr.name +
        "</option>";
    });
    response.render("placeOrder", { items: items });
  });

  app.post("/order", (request, response) => {
    let list;
    if (Array.isArray(request.body.itemsSelected)) {
      list = request.body.itemsSelected.map((curr) => {
        let arr = curr.split(",");
        return new Item(arr[0], Number(arr[1]));
      });
    } else {
      let arr = request.body.itemsSelected.split(",");
      list = [new Item(arr[0], arr[1])];
    }

    let orderTable = "<table border='1'><tr><th>Name</th><th>Cost</th></tr>";

    let total = 0.0;

    list.forEach((curr) => {
      num = Number(curr.getPrice());
      if (!isNaN(num)) {
        orderTable +=
          "<tr><td>" + curr.name + "</td><td>" + num.toFixed(2) + "</td></tr>";
        total += num;
      }
    });

    orderTable += "<tr><td>Total Cost:</td><td>" + total.toFixed(2) + "</td></tr>";

    orderTable += "</table>";

    response.render("orderConfirmation", {
      name: request.body.name,
      email: request.body.email,
      delivery: request.body.delivery,
      orderTable: orderTable,
    });
  });

  app.listen(portNumber);

  console.log(`Web server is running at http://localhost:${portNumber}`);

  const jsonFile = process.argv[2];
  let itemsList = null;
  fs.stat(jsonFile, (err, fileInfo) => {
    if (err) {
      console.error(err);
      process.exit(0);
    } else {
      let fileData = fs.readFileSync(jsonFile);
      itemsList = JSON.parse(fileData).itemsList;
    }
  });
  process.stdout.write("Type itemsList or stop to shutdown the server: ");

  process.stdin.setEncoding("utf8");
  process.stdin.on("readable", () => {
    let dataInput = process.stdin.read();
    if (dataInput !== null) {
      let command = dataInput.trim();
      if (command === "stop") {
        process.stdout.write("Shutting down the server");
        process.exit(0);
      } else if (command === "itemsList") {
        fs.stat(jsonFile, (err, fileInfo) => {
          if (err) {
            console.error(err);
          } else {
            console.log(itemsList);
            process.stdout.write("\n");
            process.stdout.write(
              "Type itemsList or stop to shutdown the server: "
            );
            process.stdin.resume();
          }
        });
      } else {
        process.stdout.write(`Invalid command: ${command}\n`);
        process.stdout.write("Type itemsList or stop to shutdown the server: ");
        process.stdin.resume();
      }
    }
  });
}

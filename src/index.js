import './main.css';
import './milligram.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import { initializeApp } from 'firebase/app';
import { getDatabase, ref, set, get, child, onValue } from "firebase/database";


// Firebase integration.
const firebaseConfig = {
  apiKey: "AIzaSyBnK2Ns0WYcecS5trjKQ56I3oX-kL1vqv4",
  authDomain: "reservation-futs.firebaseapp.com",
  databaseURL: "https://reservation-futs-default-rtdb.europe-west1.firebasedatabase.app",
  projectId: "reservation-futs",
  storageBucket: "reservation-futs.appspot.com",
  messagingSenderId: "478325705421",
  appId: "1:478325705421:web:54eef9ff9c3e752d113784"
};


const firebaseApp = initializeApp(firebaseConfig);
const db = getDatabase(firebaseApp);
const dbRef = ref(db);

onValue(ref(db, 'reservations'), (snapshot) => {
  app.ports.replaceReservations.send(JSON.stringify(snapshot.val()));
});

onValue(ref(db, 'brews'), (snapshot) => {
  app.ports.replaceBrews.send(JSON.stringify(snapshot.val()));
});

onValue(ref(db, 'inventories'), (snapshot) => {
  app.ports.replaceInventories.send(JSON.stringify(snapshot.val()));
});


var app = Elm.Main.init({
  flags: {
    reservations: localStorage.getItem('reservations') || "",
    brews: localStorage.getItem('brews') || "",
    inventories: localStorage.getItem('inventories') || "",
    seed: Math.floor(Math.random() * 0x0FFFFFFF),
  },
  node: document.getElementById('root')
});


serviceWorker.register();

app.ports.storeData.subscribe(function (data) {
  const db = getDatabase();
  set(ref(db, 'reservations'), data.reservations);
  set(ref(db, 'brews'), data.brews);
  set(ref(db, 'inventories'), data.inventories);
});

/** app.ports.messageReceiver.send(event.data); **/


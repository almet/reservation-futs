import './main.css';
import './milligram.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import { createClient } from '@supabase/supabase-js'


const getData = async function () {
  return {
    reservations: await supabase.from('storage').select('*').eq('id', 'reservations').single(),
    brews: await supabase.from('storage').select('*').eq('id', 'brews').single(),
    inventories: await supabase.from('storage').select('*').eq('id', 'inventories').single(),
  }
}

const sendToPortOnChanges = function (name, port) {
  supabase.from(`storage:id=eq.${name}`).on('UPDATE', (payload) => {
    port.send(JSON.stringify(payload.new.data));
  }).subscribe()
}

// Create a single supabase client for interacting with your database
const supabase = createClient(
  'https://wujxdtojfygcivunjwju.supabase.co',
  'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6Ind1anhkdG9qZnlnY2l2dW5qd2p1Iiwicm9sZSI6ImFub24iLCJpYXQiOjE2NjA5MDY0OTcsImV4cCI6MTk3NjQ4MjQ5N30.flobgwjv2S0ZaVs6CCGEWedx8nSYhopSWmQr_maOcrY'
)


var app = Elm.Main.init({
  flags: {
    reservations: "",
    brews: "",
    inventories: "",
    seed: Math.floor(Math.random() * 0x0FFFFFFF),
    // userId: 
  },
  node: document.getElementById('root')
});

app.ports.storeData.subscribe(function (data) {
  supabase.from('storage').update({ data: JSON.stringify(data.reservations) }).match({ id: 'reservations' }).then(console.log)
});

/* app.ports.fetchData.subscribe(async function () {
  let data = await getData();
  const val = (v) => { return v.data.data; }
  console.log(data);
  app.ports.replaceReservations(val(data.reservations));
  app.ports.replaceBrews(val(data.brews));
  app.ports.replaceInventories(val(data.inventories));
}); */

sendToPortOnChanges("reservations", app.ports.replaceReservations);
sendToPortOnChanges("brews", app.ports.replaceBrews);
sendToPortOnChanges("inventories", app.ports.replaceInventories);



/* const user = supabase.auth.user();
await supabase.auth.signIn({ email: 'contact@vieuxsinge.com' })
*/
serviceWorker.register();
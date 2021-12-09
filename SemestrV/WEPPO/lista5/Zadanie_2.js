/* Wygenerowanie certyfikatu za pomocą Keystore Explorer
 * 
 * • Tworzymy nowy zasobnik certyfikatów np. PKCS (file ⟶ new)
 * • Teraz tworzymy nową parę kluczy algorytmu RSA 
 *   Ctrl + G ⟶ RSA key size 2048 ⟶ Nadajemy nazwę, klikamy ok 
 *   ⟶ potwierdzamy alias ⟶ podajemy hasło
 * • Zapisujemy zasobnik w pliku test.pfx
 */

var https = require("https")
var fs = require('fs');

(async function() {
    let pfx = await fs.promises.readFile('test.pfx');
    https.createServer( {
        pfx: pfx,
        passphrase: 'test'
    },
    (req,res) => {
        res.end("Hello")
    }).listen(8080)
    console.log('started')
})()
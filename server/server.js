var jsonServer = require('json-server')
var _us = require('underscore')
var fs = require('fs')

// Returns an Express server
var server = jsonServer.create()

// Allow CORS with localhost
server.all("*", function(req, res, next) {
  res.header("Access-Control-Allow-Origin", "*");
  return next();
});

// Add custom routes before JSON Server router
var db = JSON.parse(fs.readFileSync('db.json'))

server.get('/random-entries', function (req, res) {
  var entries = _us.uniq(db.entries)
  res.jsonp(_us.sample(entries, 5))
})

server.get('/unauthorized', function (req, res) {
  res.sendStatus(401)
})

server.get('/not-found', function (req, res) {
  res.sendStatus(404)
})

// Use default middleware (logger, static, cors and no-cache)
var middlewares = jsonServer.defaults()
server.use(middlewares)

// Use default router
var router = jsonServer.router('db.json')
server.use(router)

server.listen(3000, function () {
  console.log()
  console.log('  🚀  Serving db.json on http://localhost:3000')
  console.log()
})

// @flow
(function () {
  'use strict'
  require('dotenv').config()
  const express = require('express')
  const path = require('path')
  const favicon = require('serve-favicon')
  const fs = require('fs-extra')
  
  const port = process.env.PORT || 4000
  const nodeEnvironment = process.env.NODE_ENV || 'development';

  if (port === undefined) {
    throw new Error(`Argument missing: port number not supplied`)
  }

  // create express server
  const app = express()

  if (nodeEnvironment === 'production') {
    // Set Security Headers.
    const helmet = require('helmet')

    app.use(helmet())

    // Content Security Policy.
    const csp = require('helmet-csp')

    // These settings should be changed (these here are just examples)!
    app.use(csp({
      directives: {
        defaultSrc: [`'none'`],
        styleSrc: [`'self'`,
          'https://fonts.googleapis.com',
          'https://www.youtube.com',
          'https://maxcdn.bootstrapcdn.com/',
          '//cdnjs.cloudflare.com'
        ],
        fontSrc: [`'self'`,
          'https://fonts.gstatic.com',
          'https://maxcdn.bootstrapcdn.com'
        ],
        scriptSrc: [`'self'`,
          'https://www.youtube.com',
          'https://www.googletagmanager.com',
          'https://www.google-analytics.com',
          'https://code.jquery.com',
          'https://maxcdn.bootstrapcdn.com',
          '//cdnjs.cloudflare.com'
        ],
        childSrc: [`'self'`, 'https://www.youtube.com'],
        imgSrc: [`'self'`,
          'www.google-analytics.com',
          'https://use.fontawesome.com',
          'https://cloud.netlifyusercontent.com'
        ],
        objectSrc: [`'none'`],
        connectSrc: [`'self'`]
      }
    }))
  }

  // Middlewares.
  // GET favicon.ico
  app.use('/', favicon(path.join('public', 'favicon.ico')))
  
  // routes for all publications page
app.get('/publications', (req, res)=>{
//	res.sendFile(path.join('public', '/publications.html'));
	res.sendFile('publications.html', { root: './public' });
})
//
// routes for project pages
  app.get('/projects/north-america', (req, res) => {
    res.sendFile("/projects/north-america.html", {
      root: './public'
    });
  });
  app.get('/projects/latin-america', (req, res) => {
    res.sendFile("/projects/latin-america.html", {
      root: './public'
    });
  });
  app.get('/projects/sub-saharan-africa', (req, res) => {
    res.sendFile("/projects/sub-saharan-africa.html", {
      root: './public'
    });
  });
app.get('/north-america', (req, res)=>{
	res.sendFile("north-america.html", { root: './public' });	
})
app.get('/latin-america', (req, res)=>{
	res.sendFile("latin-america.html", { root: './public' });	
})
app.get('/sub-saharan-africa', (req, res)=>{
	res.sendFile("sub-saharan-africa.html", { root: './public' });	
})
//

  // to serve the static files from the /public folder
  app.use('/', express.static(path.join('public')))

  app.get('*', (req, res) => {
    res.writeHead(200, { 'Content-Type': 'text/html' })
    fs.readFile(path.join('public', '404.html'), { encoding: 'utf8' }, (err, data) => {
      if (err) throw err
      res.end(data)
    })
  })

  // start the server
  app.listen(port, () => {
    console.log(`Server is listening on localhost:${port}...`)
  })
})()

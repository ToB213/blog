/*! @algolia/autocomplete-preset-algolia 1.11.1 | MIT License | © Algolia, Inc. and contributors | https://github.com/algolia/autocomplete */
!function(e,t){"object"==typeof exports&&"undefined"!=typeof module?t(exports):"function"==typeof define&&define.amd?define(["exports"],t):t((e="undefined"!=typeof globalThis?globalThis:e||self)["@algolia/autocomplete-preset-algolia"]={})}(this,(function(e){"use strict";function t(e,t){var r=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),r.push.apply(r,n)}return r}function r(e){for(var r=1;r<arguments.length;r++){var n=null!=arguments[r]?arguments[r]:{};r%2?t(Object(n),!0).forEach((function(t){i(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):t(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function n(e){return n="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(e){return typeof e}:function(e){return e&&"function"==typeof Symbol&&e.constructor===Symbol&&e!==Symbol.prototype?"symbol":typeof e},n(e)}function i(e,t,r){return(t=function(e){var t=function(e,t){if("object"!=typeof e||null===e)return e;var r=e[Symbol.toPrimitive];if(void 0!==r){var n=r.call(e,t||"default");if("object"!=typeof n)return n;throw new TypeError("@@toPrimitive must return a primitive value.")}return("string"===t?String:Number)(e)}(e,"string");return"symbol"==typeof t?t:String(t)}(t))in e?Object.defineProperty(e,t,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[t]=r,e}function o(e,t){if(null==e)return{};var r,n,i=function(e,t){if(null==e)return{};var r,n,i={},o=Object.keys(e);for(n=0;n<o.length;n++)r=o[n],t.indexOf(r)>=0||(i[r]=e[r]);return i}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(n=0;n<o.length;n++)r=o[n],t.indexOf(r)>=0||Object.prototype.propertyIsEnumerable.call(e,r)&&(i[r]=e[r])}return i}function u(e){return function(e){if(Array.isArray(e))return a(e)}(e)||function(e){if("undefined"!=typeof Symbol&&null!=e[Symbol.iterator]||null!=e["@@iterator"])return Array.from(e)}(e)||function(e,t){if(!e)return;if("string"==typeof e)return a(e,t);var r=Object.prototype.toString.call(e).slice(8,-1);"Object"===r&&e.constructor&&(r=e.constructor.name);if("Map"===r||"Set"===r)return Array.from(e);if("Arguments"===r||/^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(r))return a(e,t)}(e)||function(){throw new TypeError("Invalid attempt to spread non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.")}()}function a(e,t){(null==t||t>e.length)&&(t=e.length);for(var r=0,n=new Array(t);r<t;r++)n[r]=e[r];return n}function l(e,t){return t.reduce((function(e,t){return e&&e[t]}),e)}var c=[{segment:"autocomplete-core",version:"1.11.1"}];var s="__aa-highlight__",f="__/aa-highlight__";function p(e){var t=e.highlightedValue.split(s),r=t.shift(),n=function(){var e=arguments.length>0&&void 0!==arguments[0]?arguments[0]:[];return{get:function(){return e},add:function(t){var r=e[e.length-1];(null==r?void 0:r.isHighlighted)===t.isHighlighted?e[e.length-1]={value:r.value+t.value,isHighlighted:r.isHighlighted}:e.push(t)}}}(r?[{value:r,isHighlighted:!1}]:[]);return t.forEach((function(e){var t=e.split(f);n.add({value:t[0],isHighlighted:!0}),""!==t[1]&&n.add({value:t[1],isHighlighted:!1})})),n.get()}function g(e){var t=e.hit,r=e.attribute,n=Array.isArray(r)?r:[r],i=l(t,["_highlightResult"].concat(u(n),["value"]));return"string"!=typeof i&&(i=l(t,n)||""),p({highlightedValue:i})}var h={"&amp;":"&","&lt;":"<","&gt;":">","&quot;":'"',"&#39;":"'"},d=new RegExp(/\w/i),y=/&(amp|quot|lt|gt|#39);/g,v=RegExp(y.source);function m(e,t){var r,n,i,o=e[t],u=(null===(r=e[t+1])||void 0===r?void 0:r.isHighlighted)||!0,a=(null===(n=e[t-1])||void 0===n?void 0:n.isHighlighted)||!0;return d.test((i=o.value)&&v.test(i)?i.replace(y,(function(e){return h[e]})):i)||a!==u?o.isHighlighted:a}function b(e){return e.some((function(e){return e.isHighlighted}))?e.map((function(t,n){return r(r({},t),{},{isHighlighted:!m(e,n)})})):e.map((function(e){return r(r({},e),{},{isHighlighted:!1})}))}function O(e){var t=e.hit,r=e.attribute,n=Array.isArray(r)?r:[r],i=l(t,["_snippetResult"].concat(u(n),["value"]));return"string"!=typeof i&&(i=l(t,n)||""),p({highlightedValue:i})}function j(e,t){function n(t){return e({searchClient:t.searchClient,queries:t.requests.map((function(e){return e.query}))}).then((function(e){return e.map((function(e,r){var n=t.requests[r];return{items:e,sourceId:n.sourceId,transformResponse:n.transformResponse}}))}))}return function(e){return function(i){return r(r({requesterId:t,execute:n},e),i)}}}var A=["params"];function _(e){var t=e.searchClient,n=e.queries,i=e.userAgents,a=void 0===i?[]:i;"function"==typeof t.addAlgoliaAgent&&[].concat(u(c),u(a)).forEach((function(e){var r=e.segment,n=e.version;t.addAlgoliaAgent(r,n)}));var l=function(e){var t=e.transporter||{},r=t.headers,n=void 0===r?{}:r,i=t.queryParameters,o=void 0===i?{}:i,u="x-algolia-application-id",a="x-algolia-api-key";return{appId:n[u]||o[u],apiKey:n[a]||o[a]}}(t),p=l.appId,g=l.apiKey;return t.search(n.map((function(e){var t=e.params;return r(r({},o(e,A)),{},{params:r({hitsPerPage:5,highlightPreTag:s,highlightPostTag:f},t)})}))).then((function(e){return e.results.map((function(e,t){var i;return r(r({},e),{},{hits:null===(i=e.hits)||void 0===i?void 0:i.map((function(i){return r(r({},i),{},{__autocomplete_indexName:e.index||n[t].indexName,__autocomplete_queryID:e.queryID,__autocomplete_algoliaCredentials:{appId:p,apiKey:g}})}))})}))}))}var H=j(_,"algolia");e.createRequester=j,e.fetchAlgoliaResults=_,e.getAlgoliaFacets=function(e){n(e.searchClient);var t=H({transformResponse:function(e){return e.facetHits}}),i=e.queries.map((function(e){return r(r({},e),{},{type:"facet"})}));return t(r(r({},e),{},{queries:i}))},e.getAlgoliaResults=function(e){return n(e.searchClient),H({transformResponse:function(e){return e.hits}})(e)},e.parseAlgoliaHitHighlight=g,e.parseAlgoliaHitReverseHighlight=function(e){return b(g(e))},e.parseAlgoliaHitReverseSnippet=function(e){return b(O(e))},e.parseAlgoliaHitSnippet=O,Object.defineProperty(e,"__esModule",{value:!0})}));

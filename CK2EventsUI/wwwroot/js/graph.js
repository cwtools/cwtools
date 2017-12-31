System.register(["dagre", "cytoscape", "cytoscape-qtip", "cytoscape-dagre", "cytoscape-navigator", "cytoscape-canvas", "handlebars"], function (exports_1, context_1) {
    "use strict";
    var __moduleName = context_1 && context_1.id;
    function main(data, triggers, options, pretties, eventComment, bundleEdges) {
        var localised = new Map();
        var eventComments = new Map(eventComment);
        var getLoc = (key) => localised.has(key) ? localised.get(key) : key;
        var getName = (id) => eventComments.has(id) ? eventComments.get(id) == "" ? id : eventComments.get(id) : id;
        _data = data;
        _options = options;
        _pretty = pretties;
        cytoscape_qtip_1.default(cytoscape_1.default, $);
        cytoscape_dagre_1.default(cytoscape_1.default, dagre_1.default);
        cytoscape_canvas_1.default(cytoscape_1.default);
        var nav = cytoscape_navigator_1.default(cytoscape_1.default, $);
        var cy = cytoscape_1.default({
            container: document.getElementById('cy'),
            style: [
                {
                    selector: 'node',
                    style: {
                        'label': 'data(label)'
                    }
                },
                {
                    selector: 'edge',
                    style: {
                        'width': 3,
                        'line-color': '#ccc',
                        'mid-target-arrow-color': '#ccc',
                        'mid-target-arrow-shape': 'triangle',
                        'curve-style': bundleEdges ? 'haystack' : 'bezier',
                        'haystack-radius': 0.5
                    }
                }
            ],
            minZoom: 0.1,
            maxZoom: 5,
            layout: {
                name: 'preset',
                padding: 10
            }
        });
        var roots = [];
        var qtipname = function (text) { return { content: text, position: { my: 'top center', at: 'bottom center' }, style: { classes: 'qtip-bootstrap', tip: { width: 16, height: 8 } }, show: { event: 'mouseover' }, hide: { event: 'mouseout' } }; };
        data.forEach(function (element) {
            var name;
            name = getName(element.ID);
            var desc;
            if (element.Desc === '') {
                desc = element.ID;
            }
            else {
                desc = getLoc(element.Desc);
            }
            var node = cy.add({ group: 'nodes', data: { id: element.ID, label: name, type: element.Key, hidden: element.Hidden } });
            node.qtip(qtipname(desc));
        });
        triggers.forEach(function (event) {
            var parentID = event[0];
            event[1].forEach(function (immediates) {
                immediates.forEach(function (target) {
                    var childID = target;
                    cy.add({ group: 'edges', data: { source: parentID, target: childID } });
                });
            });
        });
        options.forEach(function (event) {
            var parentID = event[0];
            event[1].forEach(function (option) {
                var optionName = option[0][0] + "\n" + option[0][1];
                option[1].forEach(function (target) {
                    if (cy.getElementById(target).length > 0) {
                        var edge = cy.add({ group: 'edges', data: { source: parentID, target: target } });
                        if (optionName !== "") {
                            edge[0].qtip(qtipname(optionName));
                        }
                    }
                    else {
                        cy.getElementById(parentID).data('deadend_option', true);
                    }
                });
            });
        });
        cy.fit();
        var opts = { name: 'dagre', ranker: 'network-simplex' };
        cy.fit();
        var layer = cy.cyCanvas();
        var canvas = layer.getCanvas();
        var ctx = canvas.getContext('2d');
        function flatten(arr) {
            return arr.reduce(function (flat, toFlatten) {
                return flat.concat(toFlatten);
            }, []);
        }
        let toProcess = cy.elements();
        var groups = [];
        var t = cy.elements();
        groups = t.components();
        var singles = groups.filter((f) => f.length === 1);
        var singles2 = singles.reduce((p, c) => p.union(c), cy.collection());
        var rest = groups.filter((f) => f.length !== 1);
        var rest2 = rest.reduce((p, c) => p.union(c), cy.collection());
        var lrest = rest2.layout(opts);
        lrest.run();
        var bb = rest2.boundingBox({});
        var opts2 = { name: 'grid', condense: true, nodeDimensionsIncludeLabels: true };
        var lsingles = singles2.layout(opts2);
        lsingles.run();
        singles2.shift("y", (singles2.boundingBox({}).y2 + 10) * -1);
        cy.fit();
        cy.on("render", function (evt) {
            layer.resetTransform(ctx);
            layer.clear(ctx);
            layer.setTransform(ctx);
            ctx.shadowColor = "black";
            ctx.shadowBlur = 25 * cy.zoom();
            ctx.fillStyle = "#666";
            cy.nodes().forEach((node) => {
                let text = node.data('type');
                const eventChars = text.split('_').map(f => f[0].toUpperCase()).join('');
                const eventChar = text[0].toUpperCase();
                const pos = node.position();
                ctx.fillStyle = node.data('hidden') ? "#EEE" : '#888';
                ctx.beginPath();
                ctx.arc(pos.x, pos.y, 15, 0, 2 * Math.PI, false);
                ctx.fill();
                ctx.fillStyle = "black";
                ctx.stroke();
                if (node.data('deadend_option')) {
                    ctx.arc(pos.x, pos.y, 13, 0, 2 * Math.PI, false);
                    ctx.stroke();
                }
                ctx.fillStyle = "black";
                ctx.font = "16px sans-serif";
                ctx.textAlign = "center";
                ctx.textBaseline = "middle";
                ctx.fillText(eventChars, pos.x, pos.y);
            });
            ctx.restore();
        });
        var defaults = {
            container: ".cy-row",
            viewLiveFramerate: 0,
            thumbnailEventFramerate: 30,
            thumbnailLiveFramerate: false,
            dblClickDelay: 200,
            removeCustomContainer: true,
            rerenderDelay: 100
        };
        cy.on('select', 'node', function (e) {
            var node = cy.$('node:selected');
            if (node.nonempty()) {
                showDetails(node.data('id'));
            }
        });
        cy.on('select', 'edge', function (e) {
            var edges = cy.edges('edge:selected');
            var edge = edges.first();
            var opts = {};
            opts.zoom = cy.zoom();
            opts.center = { eles: edge };
            cy.animate(opts);
        });
        cy.on("resize", function (e) {
            $("#cy").width(10);
            cy.resize();
            cy.center();
        });
    }
    function showDetails(id) {
        var node = _data.filter(x => x.ID === id)[0];
        var pretty = _pretty.filter(x => x[0] === id)[0][1];
        var context = { title: node.ID, desc: node.Desc, full: pretty };
        var html = detailsTemplate(context);
        document.getElementById('detailsTarget').innerHTML = html;
    }
    exports_1("showDetails", showDetails);
    function go(filesString, bundleEdges, game) {
        document.getElementById('detailsTarget').innerHTML = "Parsing event file...";
        var files = JSON.parse(filesString);
        $.ajax({
            url: "GetData",
            data: { "files": JSON.parse(filesString), "game": game },
            contentType: "application/json"
        })
            .done(function (data) {
            main(JSON.parse(data.item2), JSON.parse(data.item3), JSON.parse(data.item4), JSON.parse(data.item5), JSON.parse(data.item6), bundleEdges);
            if (data.item1 === true) {
                document.getElementById('detailsTarget').innerHTML = "Click an event to see details";
            }
            else {
                document.getElementById('detailsTarget').innerHTML = "Failed to parse file with error(s) <br/>" + JSON.parse(data.item7);
            }
        })
            .fail(function () {
            document.getElementById('detailsTarget').innerHTML = "Something went wrong";
        });
    }
    exports_1("go", go);
    var dagre_1, cytoscape_1, cytoscape_qtip_1, cytoscape_dagre_1, cytoscape_navigator_1, cytoscape_canvas_1, handlebars_1, labelMaxLength, _data, _options, _pretty, detailsTemplate;
    return {
        setters: [
            function (dagre_1_1) {
                dagre_1 = dagre_1_1;
            },
            function (cytoscape_1_1) {
                cytoscape_1 = cytoscape_1_1;
            },
            function (cytoscape_qtip_1_1) {
                cytoscape_qtip_1 = cytoscape_qtip_1_1;
            },
            function (cytoscape_dagre_1_1) {
                cytoscape_dagre_1 = cytoscape_dagre_1_1;
            },
            function (cytoscape_navigator_1_1) {
                cytoscape_navigator_1 = cytoscape_navigator_1_1;
            },
            function (cytoscape_canvas_1_1) {
                cytoscape_canvas_1 = cytoscape_canvas_1_1;
            },
            function (handlebars_1_1) {
                handlebars_1 = handlebars_1_1;
            }
        ],
        execute: function () {
            labelMaxLength = 30;
            detailsTemplate = handlebars_1.default.compile("<h1>{{title}}</h1><div>{{desc}}</div><div></div><pre>{{full}}</pre>");
        }
    };
});

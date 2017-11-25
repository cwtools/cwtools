import dagre from 'dagre'
import cytoscape from 'cytoscape'
import cyqtip from 'cytoscape-qtip'
import cytoscapedagre from 'cytoscape-dagre'
import cytoscapenav from 'cytoscape-navigator'
import cytoscapecanvas from 'cytoscape-canvas'

declare module 'cytoscape' {
    interface CollectionElements{
        qtip(qtip:any) : any;
    }
    interface Core{
        navigator(options:any):any;
        cyCanvas():any;
    }
}




function sayHello() {
    const compiler = (document.getElementById("compiler") as HTMLInputElement).value;
    const framework = (document.getElementById("framework") as HTMLInputElement).value;
    return `Hello from ${compiler} and ${framework}!`;
}

function main(data: any, triggers: any, options: any) {
    cyqtip( cytoscape, $ );
    cytoscapedagre(cytoscape,dagre);
    cytoscapecanvas(cytoscape);
    var nav = cytoscapenav(cytoscape);

    var cy = cytoscape({
        container: document.getElementById('cy'),
        style: [ // the stylesheet for the graph
            {
                selector: 'node',
                style: {
                    //'background-color': '#666',
                    'label': 'data(id)'
                }
            },

            {
                selector: 'edge',
                style: {
                    'width': 3,
                    'line-color': '#ccc',
                    'mid-target-arrow-color': '#ccc',
                    'mid-target-arrow-shape': 'triangle',
                    'curve-style': 'bezier'
                }
            }
        ],
    })

    var roots = [];
    var qtipname = function (text:string) { return { content: text, position: { my: 'top center', at: 'bottom center' }, style: { classes: 'qtip-bootstrap', tip: { width: 16, height: 8 } }, show: { event: 'mouseover' }, hide: { event: 'mouseout' } }; }


    data.forEach(function (element : any) {
        var name = element.ID;
        var desc;
        if (element.Desc === '') {
            desc = element.ID;
        }
        else {
            desc = element.Desc;
        }
        var node = cy.add({ group: 'nodes', data: { id: name, type: element.Key } });
        node.qtip(qtipname(desc));
    });

    triggers.forEach(function (event : any) {
        var parentID = event[0];
        event[1].forEach(function (immediates : any) {
            immediates.forEach(function (target : any) {
                var childID = target;
                cy.add({ group: 'edges', data: { source: parentID, target: childID } })

            })

        })
    });
    options.forEach(function (event : any) {
        var parentID = event[0];
        event[1].forEach(function (option : any) {
            var optionName = option[0];
            option[1].forEach(function (target : any) {
                if (cy.getElementById(target).length > 0) {
                    var edge = cy.add({ group: 'edges', data: { source: parentID, target: target } });
                    if (optionName !== "") {
                        edge[0].qtip(qtipname(optionName));
                    }

                }
            })
        })
    })
    cy.fit();
    var layout = cy.layout({ name: 'dagre' });
    layout.run();
    cy.fit();

    var layer = cy.cyCanvas();
    var canvas = layer.getCanvas();
    var ctx = canvas.getContext('2d');

    cy.on("render cyCanvas.resize", function(evt) {
        layer.resetTransform(ctx);
        layer.clear(ctx);
    
    
        layer.setTransform(ctx);
    

        // Draw shadows under nodes
					ctx.shadowColor = "black";
					ctx.shadowBlur = 25 * cy.zoom();
					ctx.fillStyle = "#666";
					cy.nodes().forEach((node) => {
                        let text:string = node.data('type');
                        const eventChars = text.split('_').map(f => f[0].toUpperCase()).join('');
                        const eventChar = text[0].toUpperCase();
                        const pos = node.position();
                        //Set text to black, center it and set font.
                        ctx.fillStyle = "black";
                        ctx.font = "16px sans-serif";
                        ctx.textAlign = "center";
                        ctx.textBaseline = "middle";
                        ctx.fillText(eventChars, pos.x,pos.y);
					});
					ctx.restore();
    });
    

    var defaults = {
        container: false // can be a HTML or jQuery element or jQuery selector
      , viewLiveFramerate: 0 // set false to update graph pan only on drag end; set 0 to do it instantly; set a number (frames per second) to update not more than N times per second
      , thumbnailEventFramerate: 30 // max thumbnail's updates per second triggered by graph updates
      , thumbnailLiveFramerate: false // max thumbnail's updates per second. Set false to disable
      , dblClickDelay: 200 // milliseconds
      , removeCustomContainer: true // destroy the container specified by user on plugin destroy
      , rerenderDelay: 100 // ms to throttle rerender updates to the panzoom for performance
    };

    cy.navigator(defaults);
}

export function go(file : string){
    $.ajax({
        url: "GetData",
        data: { "file": file }
    })
        .done(function (data) {
            main(JSON.parse(data.item1), JSON.parse(data.item2), JSON.parse(data.item3));
        })
}

//go("test");

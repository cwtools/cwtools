import cytoscape from 'cytoscape'
import { } from 'cytoscape-qtip'

declare module 'cytoscape' {
    interface CollectionElements{
        qtip(qtip:any) : any;
    }
}




function sayHello() {
    const compiler = (document.getElementById("compiler") as HTMLInputElement).value;
    const framework = (document.getElementById("framework") as HTMLInputElement).value;
    return `Hello from ${compiler} and ${framework}!`;
}

function main(data: any, triggers: any, options: any) {
    var cy = cytoscape({
        container: document.getElementById('cy'),
        style: [ // the stylesheet for the graph
            {
                selector: 'node',
                style: {
                    'background-color': '#666',
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
        var node = cy.add({ group: 'nodes', data: { id: name } });
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

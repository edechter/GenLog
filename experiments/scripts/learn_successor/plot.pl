
:- module(plot, [show/0,
                 plot_xy/2,
                 plot_xy/3]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).

server(Port) :-
        http_server(http_dispatch,
                    [ port(Port)
                    ]).

% :- http_handler(root(.), gen_plot_xy([1,2], [1, 2]), []).
:- http_handler(root('c3.css'),    http_reply_file('./resources/c3/c3.css', []), []).
:- http_handler(root('c3.min.js'), http_reply_file('./resources/c3/c3.min.js', []), []).

                                                   
:- html_resource(c3,
                 [ requires([
                              'https://cdnjs.cloudflare.com/ajax/libs/d3/3.5.6/d3.min.js',
                              root('c3.min.js'),
                              root('c3.css')
                            ]),
                   ordered(true),
                   virtual(true)
                 ]).


close_server :-
        catch(http_stop_server(8000, []),
              _,
              true).

show :-
        close_server,
        server(8000),
        www_open_url('http://localhost:8000').

plot_xy(X, Y) :-
        plot_xy(X, Y, []).

plot_xy(X, Y, Options) :-
        http_handler(root(.),
                     gen_plot_xy(X, Y, Options),
                     []).
        


gen_plot_xy(Xs, Ys, Options, _Request) :-
        reply_html_page(
            title('Plot'),
            \gen_plot_xy_(Xs, Ys, Options)).

gen_plot_xy_(Xs, Ys, Options) -->
        html_requires(c3),
        html([ h1('Chart'),
               div(id(plot), [])
             ]),
        { pairs_keys_values(XYs, Xs, Ys),
          findall(
                  [X,Y],
                  member(X-Y, XYs),
                  Data),
          XCol = [ "x" | Xs ],
          YCol = [ "y" | Ys ]
        },
        js_script({|javascript(XCol, YCol)||
                   c3.generate({
                     bindto: "#plot",
                     data: { x: "x",
                             columns: [XCol, YCol]
                           },
                     axis: {
                            y : {
                                 min: 0,
                                 max: 1
                                }
                           
                                }
                   });
                  |}).


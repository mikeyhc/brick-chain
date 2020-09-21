-record(brick, {hash :: binary(),
                entries :: [iodata()],
                previous :: binary() | undefined,
                signature :: binary()
               }).
-type brick() :: #brick{}.

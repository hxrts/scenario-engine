-- | The pages anyone can open: the landing page and the admin login. They read no data; the password goes to
-- | the server as a form post.
module Dashboard.Public (mkLanding, mkLogin) where

import Prelude

import Dashboard.Layout as Layout
import Dashboard.Theme (themeToggle)
import Dashboard.UI as UI
import React.Basic.DOM as D
import React.Basic.Hooks (Component, JSX, component, element)

navLink :: String -> String -> JSX
navLink href label = D.a { href, className: "text-muted-foreground hover:text-foreground", children: [ D.text label ] }

publicShell :: JSX -> JSX -> JSX
publicShell toggle page = D.div
  { className: "min-h-screen"
  , children:
      [ Layout.topBar { brand: "Scenario Engine", home: "/", nav: [ navLink "/admin/" "Admin" ], themeButton: toggle }
      , D.main { className: "min-w-0", children: [ page ] }
      ]
  }

-- | `/`: what the game is, for someone about to play it.
mkLanding :: Component Unit
mkLanding = do
  toggle <- themeToggle
  component "Landing" \_ -> pure $ publicShell (toggle unit) $ Layout.page
    { title: "Scenario Planning Game"
    , purpose: "You are playing an influential actor in a shared world economy."
    , children:
        [ D.p { className: "max-w-prose", children: [ D.text "Over a few rounds you will negotiate, trade and make decisions, and see how they shape what happens next." ] }
        , D.p { className: "max-w-prose text-muted-foreground", children: [ D.text "A description of the game's premises will appear here. For now, your facilitator will give you the link to your own balance sheet." ] }
        ]
    }

-- | `/admin` without a session. `failed` is the server's word that the last password was wrong.
mkLogin :: Component { failed :: Boolean }
mkLogin = do
  toggle <- themeToggle
  component "Login" \{ failed } -> pure $ publicShell (toggle unit) $ D.div
    { className: "mx-auto flex w-full max-w-sm flex-col px-gutter py-section"
    , children:
        [ element UI.card
            { children:
                [ element UI.cardHeader { children: [ element UI.cardTitle { children: [ D.text "Admin" ] } ] }
                , element UI.cardContent
                    { children:
                        [ D.form
                            { method: "post"
                            , action: "/admin/login"
                            , className: "flex flex-col gap-stack"
                            , children:
                                (if failed then [ D.p { role: "alert", className: "text-sm text-destructive", children: [ D.text "That is not the password." ] } ] else [])
                                  <>
                                    [ D.label { htmlFor: "password", className: "text-sm font-medium", children: [ D.text "Password" ] }
                                    , element UI.input { id: "password", name: "password", type: "password", required: true, autoFocus: true, autoComplete: "current-password" }
                                    , element UI.button { type: "submit", children: [ D.text "Sign in" ] }
                                    ]
                            }
                        ]
                    }
                ]
            }
        ]
    }

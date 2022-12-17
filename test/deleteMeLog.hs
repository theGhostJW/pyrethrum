romList
  [ ( RTLoc
        { loc = Node { parent = Root , tag = "ThreadHook" }
        , nodeType = Threaded SThreadId { display = "ThreadId 1984" }
        }
    , [ RTLocEvt
          { rtLoc =
              RTLoc
                { loc =
                    Node
                      { parent = Node { parent = Root , tag = "ThreadHook" }
                      , tag = "ThreadHookRelease"
                      }
                , nodeType = Threaded SThreadId { display = "ThreadId 1984" }
                }
          , eventType = ThreadHookRelease
          }
      , RTLocEvt
          { rtLoc =
              RTLoc
                { loc =
                    Node
                      { parent = Node { parent = Root , tag = "ThreadHook" }
                      , tag = "OnceHook"
                      }
                , nodeType = Singleton
                }
          , eventType = OnceHook
          }
      ]
    )
  , ( RTLoc
        { loc =
            Node
              { parent = Node { parent = Root , tag = "ThreadHook" }
              , tag = "OnceHook"
              }
        , nodeType = Singleton
        }
    , [ RTLocEvt
          { rtLoc =
              RTLoc
                { loc =
                    Node
                      { parent =
                          Node
                            { parent = Node { parent = Root , tag = "ThreadHook" }
                            , tag = "OnceHook"
                            }
                      , tag = "OnceHookRelease"
                      }
                , nodeType = Singleton
                }
          , eventType = OnceHookRelease
          }
      , RTLocEvt
          { rtLoc =
              RTLoc
                { loc =
                    Node
                      { parent =
                          Node
                            { parent = Node { parent = Root , tag = "ThreadHook" }
                            , tag = "OnceHook"
                            }
                      , tag = "Fixture 0"
                      }
                , nodeType = Threaded SThreadId { display = "ThreadId 1984" }
                }
          , eventType = Fixture
          }
      ]
    )
  , ( RTLoc
        { loc =
            Node
              { parent = Node { parent = Root , tag = "ThreadHook" }
              , tag = "ThreadHookRelease"
              }
        , nodeType = Threaded SThreadId { display = "ThreadId 1984" }
        }
    , []
    )
  , ( RTLoc
        { loc =
            Node
              { parent =
                  Node
                    { parent = Node { parent = Root , tag = "ThreadHook" }
                    , tag = "OnceHook"
                    }
              , tag = "Fixture 0"
              }
        , nodeType = Threaded SThreadId { display = "ThreadId 1984" }
        }
    , [ RTLocEvt
          { rtLoc =
              RTLoc
                { loc =
                    Node
                      { parent =
                          Node
                            { parent =
                                Node
                                  { parent = Node { parent = Root , tag = "ThreadHook" }
                                  , tag = "OnceHook"
                                  }
                            , tag = "Fixture 0"
                            }
                      , tag = "FixtureOnceHook"
                      }
                , nodeType = Singleton
                }
          , eventType = FixtureOnceHook
          }
      ]
    )
  , ( RTLoc
        { loc =
            Node
              { parent =
                  Node
                    { parent = Node { parent = Root , tag = "ThreadHook" }
                    , tag = "OnceHook"
                    }
              , tag = "OnceHookRelease"
              }
        , nodeType = Singleton
        }
    , []
    )
  , ( RTLoc
        { loc =
            Node
              { parent =
                  Node
                    { parent =
                        Node
                          { parent = Node { parent = Root , tag = "ThreadHook" }
                          , tag = "OnceHook"
                          }
                    , tag = "Fixture 0"
                    }
              , tag = "FixtureOnceHook"
              }
        , nodeType = Singleton
        }
    , [ RTLocEvt
          { rtLoc =
              RTLoc
                { loc =
                    Node
                      { parent =
                          Node
                            { parent =
                                Node
                                  { parent =
                                      Node
                                        { parent = Node { parent = Root , tag = "ThreadHook" }     
                                        , tag = "OnceHook"
                                        }
                                  , tag = "Fixture 0"
                                  }
                            , tag = "FixtureOnceHook"
                            }
                      , tag = "FixtureOnceHookRelease"
                      }
                , nodeType = Singleton
                }
          , eventType = FixtureOnceHookRelease
          }
      , RTLocEvt
          { rtLoc =
              RTLoc
                { loc =
                    Node
                      { parent =
                          Node
                            { parent =
                                Node
                                  { parent =
                                      Node
                                        { parent = Node { parent = Root , tag = "ThreadHook" }     
                                        , tag = "OnceHook"
                                        }
                                  , tag = "Fixture 0"
                                  }
                            , tag = "FixtureOnceHook"
                            }
                      , tag = "FixtureThreadHook"
                      }
                , nodeType = Threaded SThreadId { display = "ThreadId 1984" }
                }
          , eventType = FixtureThreadHook
          }
      ]
    )
  , ( RTLoc
        { loc =
            Node
              { parent =
                  Node
                    { parent =
                        Node
                          { parent =
                              Node
                                { parent = Node { parent = Root , tag = "ThreadHook" }
                                , tag = "OnceHook"
                                }
                          , tag = "Fixture 0"
                          }
                    , tag = "FixtureOnceHook"
                    }
              , tag = "FixtureOnceHookRelease"
              }
        , nodeType = Singleton
        }
    , []
    )
  , ( RTLoc
        { loc =
            Node
              { parent =
                  Node
                    { parent =
                        Node
                          { parent =
                              Node
                                { parent = Node { parent = Root , tag = "ThreadHook" }
                                , tag = "OnceHook"
                                }
                          , tag = "Fixture 0"
                          }
                    , tag = "FixtureOnceHook"
                    }
              , tag = "FixtureThreadHook"
              }
        , nodeType = Threaded SThreadId { display = "ThreadId 1984" }
        }
    , [ RTLocEvt
          { rtLoc =
              RTLoc
                { loc =
                    Node
                      { parent =
                          Node
                            { parent =
                                Node
                                  { parent =
                                      Node
                                        { parent =
                                            Node
                                              { parent = Node { parent = Root , tag =
"ThreadHook" }
                                              , tag = "OnceHook"
                                              }
                                        , tag = "Fixture 0"
                                        }
                                  , tag = "FixtureOnceHook"
                                  }
                            , tag = "FixtureThreadHook"
                            }
                      , tag = "FixtureOnceHookRelease"
                      }
                , nodeType = Threaded SThreadId { display = "ThreadId 1984" }
                }
          , eventType = FixtureThreadHookRelease
          }
      , RTLocEvt
          { rtLoc =
              RTLoc
                { loc =
                    Node
                      { parent =
                          Node
                            { parent =
                                Node
                                  { parent =
                                      Node
                                        { parent =
                                            Node
                                              { parent = Node { parent = Root , tag =
"ThreadHook" }
                                              , tag = "OnceHook"
                                              }
                                        , tag = "Fixture 0"
                                        }
                                  , tag = "FixtureOnceHook"
                                  }
                            , tag = "FixtureThreadHook"
                            }
                      , tag = "TestHook :: 0"
                      }
                , nodeType = Threaded SThreadId { display = "ThreadId 1984" }
                }
          , eventType = TestHook
          }
      ]
    )
  , ( RTLoc
        { loc =
            Node
              { parent =
                  Node
                    { parent =
                        Node
                          { parent =
                              Node
                                { parent =
                                    Node
                                      { parent = Node { parent = Root , tag = "ThreadHook" }       
                                      , tag = "OnceHook"
                                      }
                                , tag = "Fixture 0"
                                }
                          , tag = "FixtureOnceHook"
                          }
                    , tag = "FixtureThreadHook"
                    }
              , tag = "FixtureOnceHookRelease"
              }
        , nodeType = Threaded SThreadId { display = "ThreadId 1984" }
        }
    , []
    )
  , ( RTLoc
        { loc =
            Node
              { parent =
                  Node
                    { parent =
                        Node
                          { parent =
                              Node
                                { parent =
                                    Node
                                      { parent = Node { parent = Root , tag = "ThreadHook" }       
                                      , tag = "OnceHook"
                                      }
                                , tag = "Fixture 0"
                                }
                          , tag = "FixtureOnceHook"
                          }
                    , tag = "FixtureThreadHook"
                    }
              , tag = "TestHook :: 0"
              }
        , nodeType = Threaded SThreadId { display = "ThreadId 1984" }
        }
    , [ RTLocEvt
          { rtLoc =
              RTLoc
                { loc =
                    Node
                      { parent =
                          Node
                            { parent =
                                Node
                                  { parent =
                                      Node
                                        { parent =
                                            Node
                                              { parent =
                                                  Node
                                                    { parent =
                                                        Node { parent = Root , tag = "ThreadHook"  
}
                                                    , tag = "OnceHook"
                                                    }
                                              , tag = "Fixture 0"
                                              }
                                        , tag = "FixtureOnceHook"
                                        }
                                  , tag = "FixtureThreadHook"
                                  }
                            , tag = "TestHook :: 0"
                            }
                      , tag = "TestHookRelease :: 0"
                      }
                , nodeType = Threaded SThreadId { display = "ThreadId 1984" }
                }
          , eventType = TestHookRelease
          }
      , RTLocEvt
          { rtLoc =
              RTLoc
                { loc =
                    Node
                      { parent =
                          Node
                            { parent =
                                Node
                                  { parent =
                                      Node
                                        { parent =
                                            Node
                                              { parent =
                                                  Node
                                                    { parent =
                                                        Node { parent = Root , tag = "ThreadHook"  
}
                                                    , tag = "OnceHook"
                                                    }
                                              , tag = "Fixture 0"
                                              }
                                        , tag = "FixtureOnceHook"
                                        }
                                  , tag = "FixtureThreadHook"
                                  }
                            , tag = "TestHook :: 0"
                            }
                      , tag = "Test :: 0"
                      }
                , nodeType = Threaded SThreadId { display = "ThreadId 1984" }
                }
          , eventType = Test
          }
      ]
    )
  , ( RTLoc
        { loc =
            Node
              { parent =
                  Node
                    { parent =
                        Node
                          { parent =
                              Node
                                { parent =
                                    Node
                                      { parent =
                                          Node
                                            { parent = Node { parent = Root , tag = "ThreadHook" } 
                                            , tag = "OnceHook"
                                            }
                                      , tag = "Fixture 0"
                                      }
                                , tag = "FixtureOnceHook"
                                }
                          , tag = "FixtureThreadHook"
                          }
                    , tag = "TestHook :: 0"
                    }
              , tag = "Test :: 0"
              }
        , nodeType = Threaded SThreadId { display = "ThreadId 1984" }
        }
    , []
    )
  , ( RTLoc
        { loc =
            Node
              { parent =
                  Node
                    { parent =
                        Node
                          { parent =
                              Node
                                { parent =
                                    Node
                                      { parent =
                                          Node
                                            { parent = Node { parent = Root , tag = "ThreadHook" } 
                                            , tag = "OnceHook"
                                            }
                                      , tag = "Fixture 0"
                                      }
                                , tag = "FixtureOnceHook"
                                }
                          , tag = "FixtureThreadHook"
                          }
                    , tag = "TestHook :: 0"
                    }
              , tag = "TestHookRelease :: 0"
              }
        , nodeType = Threaded SThreadId { display = "ThreadId 1984" }
        }
    , []
    )
  ]
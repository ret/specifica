module Parser
  (shortspec, mkState)
where

import Data.Char (toLower, toUpper, isAlpha)
import Debug.Trace as Trace
import System (getArgs)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language( emptyDef )
import Text.ParserCombinators.Parsec.Pos(SourcePos(..))
import qualified Text.ParserCombinators.Parsec.Token as P
import List (nub, (\\))
import Data.Set as Set (fromList)

import Language.TLAPlus.ParserState 
         (TLAParser, PState, mkState
	 , setRole, getRole
	 , setInteraction, getInteraction)
import Language.TLAPlus.Syntax 
         (AS_Expression(..)) -- AS_Ident, AS_DiscreteSet
import Syntax

import Language.TLAPlus.Parser as TLAParser -- for expression parsing

reserved s =     reservedX s               -- as is, e.g. "Set" for types
             <|> reservedX (map toLower s) -- all lower case keywords
             <|> reservedX (map toUpper s) -- all upper case keywords

shortspec :: TLAParser SH_Spec
shortspec = do { whiteSpace
	       ; reserved "PROTOCOL"
	       ; p <- identifier
	       ; c <- do{ p <- getPosition
			; cname <- option "default" $
			              do { reserved "CONCERN"
					 ; c <- identifier <?> "concern name"
					 ; return c
					 }
			; reservedOp "{"
			; l <- sepBy concernEl whiteSpace
			; reservedOp "}"
			; return $ SH_Concern p cname l
			}
	       ; return SH_Spec{ protocol = p,
				 concernList = [c]
			       }
	       }

specEnd :: TLAParser ()
specEnd = do{ char '#' <?> "end of protocol (#)"
	    ; whiteSpace
	    }

---- CONCERN ------------------------------------------------------------------
concern :: TLAParser SH_Concern
concern = do{ p <- getPosition
	    ; reserved "CONCERN"
	    ; c <- identifier <?> "concern name"
	    ; reservedOp "{"
	    ; l <- sepBy concernEl whiteSpace
	    ; reservedOp "}"
	    ; return $ SH_Concern p c l
	    }

concernEl :: TLAParser SH_ConcernElement
concernEl = do{ e <- choice [ constant
		            , roleList
			    , interaction
			    ]
	      ; return e
	      }

constant :: TLAParser SH_ConcernElement
constant = do{ p <- getPosition
	     ; reserved "CONSTANT" <|> reserved "CONSTANTS"
	     ; l <- commaSep identifier
	     ; return $ SH_Constant p l
	     }

roleList :: TLAParser SH_ConcernElement
roleList = do{ p <- getPosition
	     ; reserved "ROLES"
	     ; l <- commaSep identifier
	     ; return $ SH_RoleList p l
	     }

---- INTERACTION --------------------------------------------------------------
interaction :: TLAParser SH_ConcernElement
interaction = do{ p <- getPosition
		; e <- option True 
		         (do { (reserved "DISABLE" <|> reserved "DISABLED")
			     ; return False
			     })
		; reserved "INTERACTION"
		; name <- identifier
		; st <- getState
		; let st' = setInteraction st name -- over VerbTLA override 
		; setState st'
		; roles <- option [] 
		             (parens $ commaSep ty) {- roles, incl SET<> -}
		; reservedOp "{"
		; l <- sepBy interactionEl whiteSpace
		; reservedOp "}"
		; return $ SH_Interaction p name e roles l
		}

msgDecl :: TLAParser SH_MsgDecl
msgDecl = do{ p <- getPosition
	    ; reserved "MSG"
	    ; r1 <- ty
	    ; reservedOp "->"
	    ; r2 <- ty
	    ; t <- identifier {-type-}
	    ; l <- msgType
	    ; return $ SH_MsgDecl p r1 r2 t l
	    }

extendMsg :: TLAParser SH_InteractionElement
extendMsg = do{ p <- getPosition
	      ; reserved "EXTEND"
	      ; m <- msgDecl
	      ; let (SH_MsgDecl _ from to mtype l) = m
	      ; return $ SH_Extend_Msg p from to mtype l
	      }

interactionEl :: TLAParser SH_InteractionElement
interactionEl = do{ p <- getPosition
		  ; e <- choice [ do { m <- msgDecl
				     ; return $ SH_IntraInteractionMsg p m }
				, extendMsg
				, roledef
				, verbTLA
				, displaySL] -- add try if more than sl
		  ; return e
		  }

verbTLA :: TLAParser SH_InteractionElement
verbTLA = do{ p <- getPosition
	    ; o <- option Nothing
	             (do { reserved "OVERRIDE"
			 ; li <- commaSep identifier
			 ; return $ Just li
			 })
	    ; st <- getState
	    ; let int = getInteraction st
	    ; reserved "TLA"
	    ; reservedOp "{"
	    ; op <- TLAParser.operatorDef
	    ; reservedOp "}"
	    ; return $ SH_VerbTLAOp p int o op
	    }

displaySL :: TLAParser SH_InteractionElement
displaySL = do { p <- getPosition
	       ; reserved "DISPLAY"
	       ; reserved "SWIMLANE"
	       ; reservedOp "{"
	       ; l <- sepBy displaySLEntry whiteSpace
	       ; reservedOp "}"
	       ; return $ SH_DisplaySwimlane p l
	       }

displaySLEntry :: TLAParser SH_SL_Ann
displaySLEntry = do { p <- getPosition
		    ; reserved "MSG"
		    ; mtype <- identifier
		    ; l <- commaSep 
		                 (do { k <-     do { reserved "COLOR"
					 	   ; return SH_SL_MsgAnnColor
						   }
				            <|> do { reserved "STYLE"
						   ; return SH_SL_MsgAnnStyle
						   }
				     ; reservedOp "="
				     ; e <- expr
				     ; return (k, e) 
				     })
		    ; return $ SH_SL_MsgAnn mtype l
		    }


---- ROLE ---------------------------------------------------------------------
roledef :: TLAParser SH_InteractionElement
roledef = do{ p <- getPosition
	    ; reserved "ROLE"
	    ; name <- identifier
	    ; st <- getState
	    ; let st' = setRole st name -- role for elements and instr
	    ; setState st'
	    ; paramlist <- option []
	                     (parens $ commaSep (do { t <- ty
						    ; i <- identifier 
						    ; return (t,i) 
						    }))
	    ; reservedOp "{"
	    ; l <- roleEl
	    ; reservedOp "}"
	    ; return $ SH_RoleDef p name paramlist l
	    }

roleEl :: TLAParser [SH_RoleElement]
roleEl = do{ l <- sepBy (choice [ try whenblock
				, try requires
				, try view  -- starts with STATE also
				, try timer -- start with STATE also
				, try state -- genuine STATE value
                                , try callhandler
				, try (msghandler True) -- HANDLE
				, try timeouthandler
				, try crashhandler
				, try every
				, try extendHook
				, try use
				, taghandler
				, oncehandler
		               ])
		         whiteSpace
	 ; return l
	 }

use :: TLAParser SH_RoleElement 
use = do{ p <- getPosition
	; reserved "USE"
	; e <-     do { reserved "STATE"
		      ; statefield <- identifier
		      ; reserved "OF"
		      ; interact <- identifier
		      ; return $ SH_UseState p statefield interact
		      }
               <|> do { reserved "MSG"
		      ; t <- identifier
		      ; reserved "OF"
		      ; interact <- identifier
		      ; return $ SH_UseMsg p t interact
		      }
	; return e
	}

requires :: TLAParser SH_RoleElement
requires = do{ p <- getPosition
	     ; reserved "REQUIRE" <|> reserved "REQUIRES"
	     ; e <- expr
	     ; return $ SH_Require p e
	     }

state :: TLAParser SH_RoleElement
state = do{ p <- getPosition
	  ; per <- option False (do { reserved "PERSISTENT"
				    ; return True
				    })
	  ; reserved "STATE"
	  ; t <- ty
	  ; id <- identifier {-type-}
	  ; init <- option Nothing (do { reservedOp "="
				       ; pp <- getPosition
				       ; init <- expr
				       ; return $ Just init
				       })
	  ; return $ SH_State p per (t,id) init
	  }

view :: TLAParser SH_RoleElement
view = do{ p <- getPosition
	 ; reserved "STATE"
	 ; reserved "VIEW" <|> reserved "VIEWS"
	 ; t <- ty
	 ; reservedOp "="
	 ; init <- expr {- initial view, must be passed into ROLE -}
	 ; return $ SH_ViewState p t init
	 }

timer :: TLAParser SH_RoleElement
timer = do{ p <- getPosition
	  ; reserved "STATE"
	  ; reserved "TIMER"
	  ; id <- identifier {-name-}
	  ; st <- getState
	  ; let role = getRole st
	  ; return $ SH_Timer p role id
	  }

msgType :: TLAParser [(SH_Type, String)]
msgType = parens $ commaSep (do { t <- ty
				; whiteSpace
				; i <- identifier
				; return $ (t,i)
				})
	    
callhandler :: TLAParser SH_RoleElement
callhandler = do { p <- getPosition
		 ; guard <- try $ option Nothing whenGuard
		 ; reserved "HANDLE" <|> reserved "HANDLES" 
		 ; whiteSpace
		 ; reserved "EVENT"
		 ; label <- identifier
		 ; msgrec <- option [] 
		               (do { m <- parens $ commaSep 
				            (do { t <- identifier
						; i <- identifier
						; return (t,i) }) 
				   ; return $ m })
		 ; hook <- option Nothing (do { l <- commaSep hookCaller
					      ; return $ Just l})
		 ; l <- handlerBody
		 ; st <- getState
		 ; let role = getRole st
		 ; let lowRole = map toLower role
		 ; return $ SH_CallHandler p lowRole guard label msgrec hook l
		 }


msghandler :: Bool -> TLAParser SH_RoleElement
msghandler inHandle = 
  do { p <- getPosition
     ; ann <- option [] handlerannList
     ; guard <- option Nothing whenGuard
     ; if inHandle 
       then reserved "HANDLE" <|> reserved "HANDLES"
       else reserved "AWAIT"
     ; any <- option False (do { reserved "ANY"
			       ; return True })
     ; reserved "MSG" <|> reserved "MSGS"
     ; m <- identifier
     ; from_scope <- option Nothing 
       (do { reserved "FROM"
	   ; scope <- (do { reserved "ALL"
			  ; v <- parens identifier
			  ; w <- whereQual
			  ; return $ Just (SH_FromAll v, w)}) 
	          <|> (do { reserved "MAJORITY"
			  ; v <- parens identifier
			  ; w <- whereQual
			  ; return $ Just (SH_FromMaj v, w)})
	          <|> (do { reserved "EXPR"
			  ; (t,e) <- parens (do { t <- identifier
						; reservedOp ","
						; e <- expr
						; return (t,e)
						})
			  ; w <- whereQual
			  ; return $ Just (SH_FromExp t e, w)})
	   ; return scope
	   })
     ; hook <- option Nothing (do { l <- commaSep hookCaller
				  ; return $ Just l})
     ; l <- if inHandle 
            then handlerBody 
            else return []
     ; st <- getState
     ; let role = getRole st
     ; let lowRole = map toLower role
     ; return $ SH_MsgHandler p ann lowRole 
         guard m hook any from_scope l
     }

whereQual :: TLAParser (Maybe (SH_ExprWrapper, SH_WhereQuantifierKind))
whereQual = do { w <- option Nothing
	                     (do { reserved "WHERE"
				 ; q <-     do { reserved "ALL" 
				               ; return SH_All } 
				        <|> do { reserved "SOME" 
				               ; return SH_Some }
				        <|> do { reserved "NONE" 
				               ; return SH_None }
				 ; e <- expr
				 ; return $ Just (e, q)
				 })
	       ; return w
	       }

timeouthandler :: TLAParser SH_RoleElement
timeouthandler = do { p <- getPosition
		    ; guard <- try $ option Nothing whenGuard
		    ; reserved "HANDLE" <|> reserved "HANDLES" 
		    ; reserved "TIMEOUT" <|> reserved "TIMEOUTS"
		    ; t <- identifier
		    ; hook <- option Nothing (do { l <- commaSep hookCaller
						 ; return $ Just l})
		    ; l <- handlerBody
		    ; st <- getState
		    ; let role = getRole st
		    ; let lowRole = map toLower role
		    ; return $ SH_TimeoutHandler p lowRole guard t hook l
		    }

crashhandler :: TLAParser SH_RoleElement
crashhandler = do { p <- getPosition
		  ; ann <- option [] handlerannList
		  ; guard <- try $ option Nothing whenGuard
		  ; reserved "HANDLE" <|> reserved "HANDLES" 
		  ; reserved "CRASH"
		  ; t <- identifier
		  ; id <- identifier
		  ; hook <- option Nothing (do { l <- commaSep hookCaller
					       ; return $ Just l})
		  ; l <- handlerBody
		  ; st <- getState
		  ; let role = getRole st
		  ; let lowRole = map toLower role
		  ; return $ SH_CrashHandler p ann lowRole guard t id hook l
		  }

every :: TLAParser SH_RoleElement
every = do { p <- getPosition
	   ; reserved "EVERY"
	   ; e <- expr
	   ; guard <- try $ option Nothing untilGuard
	   ; hook <- option Nothing (do { l <- commaSep hookCaller
					; return $ Just l})
	   ; l <- handlerBody
	   ; st <- getState
	   ; let role = getRole st
	   ; let lowRole = map toLower role
	   ; return $ SH_Every p role guard e hook l
	   }

extendHook :: TLAParser SH_RoleElement
extendHook = do{ p <- getPosition
	       ; reserved "EXTEND"
	       ; hl <- commaSep hookCallee
	       ; l <- handlerBody
	       ; st <- getState
	       ; let role = getRole st
	       ; let lowRole = map toLower role
	       ; return $ SH_Extend_Hook p role hl l
	       }

hookCaller :: TLAParser SH_HookCaller
hookCaller = do{ p <- getPosition
	       ; reservedOp "@"
	       ; label <- identifier
	       ; l <- option [] (parens $ commaSep expr)
	       ; return $ SH_HookCaller p label l
	       }
 
hookCallee :: TLAParser SH_HookCallee
hookCallee = do{ p <- getPosition
	       ; reservedOp "@"
	       ; label <- identifier
	       ; l <- option [] (parens $ commaSep identifier)
	       ; return $ SH_HookCallee p label l
	       }

taghandler :: TLAParser SH_RoleElement
taghandler = do { p <- getPosition
		; reserved "TAG" <|> reserved "TAGS"
		; any <- option False (do { reserved "ANY"
					  ; return True })
		; reserved "MSG" <|> reserved "MSGS"
		; m <- commaSep identifier
		; reserved "TO"
		; destrole <- ty
		; reservedOp "{"
		; ass <- commaSep (do { t <- ty
				      ; i <- identifier
				      ; reservedOp "="
				      ; e <- expr
				      ; return $ ((t,i),e)
				      })
		; reserved "}"
		; st <- getState
		; let role = getRole st
		; return $ SH_Tag p role m any destrole ass
		}

oncehandler :: TLAParser SH_RoleElement
oncehandler = do { p <- getPosition
		 ; guard <- try $ option Nothing whenGuard
		 ; reserved "HANDLE" <|> reserved "HANDLES" 
		 ; reserved "ONCE" <|> reserved "ONCE" 
		 ; label <- identifier
		 ; hook <- option Nothing (do { l <- commaSep hookCaller
					      ; return $ Just l})
		 ; l <- handlerBody
		 ; st <- getState
		 ; let role = getRole st
		 ; let lowRole = map toLower role
		 ; return $ SH_Once p lowRole guard label hook l
		 }

whenblock :: TLAParser SH_RoleElement
whenblock = do { p <- getPosition
	       ; reserved "WHEN"
	       ; guard <- expr
	       ; reserved "{"
	       ; l <- roleEl
	       ; reserved "}"
	       ; return $ SH_WhenBlock p guard l
	       }

handlerannList :: TLAParser [HandlerAnnotation]
handlerannList = do { p <- getPosition
		    ; reserved "USING"
		    ; l <-     do { reservedOp "["
				  ; l <- commaSep handlerann
				  ; reservedOp "]"
				  ; return l
				  }
		           <|> do { a <- handlerann 
				  ; return $ [a] 
				  }
		    ; return l
		    }

handlerann :: TLAParser HandlerAnnotation
handlerann = do { p <- getPosition
		; a <- identifier
		; l <- option [] (parens $ commaSep identifier)
		; return $ HandlerAnnotation a l
		}

---- INSTRUCTIONS -------------------------------------------------------------
handlerBody :: TLAParser [SH_GuardedInstrList]
handlerBody = do { reserved "{"
		 ; l <- sepBy1 guardedInstrList whiteSpace
		 ; reserved "}"
		 ; return l
		 }

guardedInstrList :: TLAParser SH_GuardedInstrList
guardedInstrList = do{ p <- getPosition
		     ; g <- option Nothing (do { reservedOp "|"
					       ; guard <- expr
					       ; reservedOp "->"
					       ; return $ Just guard
					       })
		     ; label <- option Nothing (do { l <- commaSep hookCaller
						   ; return $ Just l})
		     ; l <- instrList
		     ; return $ SH_GuardedInstrList p g label l
		     }

whenGuard :: TLAParser (Maybe SH_ExprWrapper)
whenGuard = do { reserved "WHEN"
	       ; guard <- expr
	       ; return $ Just guard
	       }

untilGuard :: TLAParser (Maybe SH_ExprWrapper)
untilGuard = do { reserved "WHILE"
		; guard <- expr
		; return $ Just guard
		}

expr :: TLAParser SH_ExprWrapper
expr = do { p <- getPosition
	  ; e <- TLAParser.expression
	  ; return $ SH_ExprWrapper p e
	  }

instrList :: TLAParser [SH_Instr]
instrList = sepBy1 (choice [
		     instrChangeState
		   , instrChangeView
	           , instrTimerrestart
	           , instrTimercancel
	           , instrShutdown
		   , instrDrop
		   , instrReply
		   , instrAssert
	           , instrForeignChangeState -- only intended for code gen use
		   , instrAwait
		   , instrWhenBlockedIn
	           , try instrLet
		   , try instrMsgSend1 -- no not eat a in (a ! b), may be END
		   , try instrBreak
		   , try instrContinue
		   , try instrRewind
		   , try instrState
                   ])
              whiteSpace

viewOrExpr :: TLAParser SH_ExprWrapper
viewOrExpr = do{ p <- getPosition
	       ; e <-     try (do{ reserved "VIEW"
				 ; r <- parens identifier
				 ; return $ SH_VIEW_REF p r
				 })
                      <|> try expr
	       ; return e
	       }


instrMsgSend1 :: TLAParser SH_Instr
instrMsgSend1 = do { p <- getPosition
		   ; dest <- viewOrExpr
{------------- TLC EXTENSION ---------------}
                   -- ! is not a TLC op - see FIXME in qualindent
{------------- TLC EXTENSION ---------------}
		   ; m <-     do { reservedOp "!"; return False }
		          <|> do { reservedOp "!!"; return True }
		   ; t <- identifier
		   ; msgrec <- option [] (do { m <- msgRecord
					     ; return m })
		   ; st <- getState
		   ; let role = getRole st
		   ; return $ SH_I_MsgSend1 p role m False dest t msgrec
		   }

msgRecord :: TLAParser [(String, SH_ExprWrapper)]
msgRecord = parens $ commaSep (do { i <- identifier
				  ; reservedOp "="
				  ; e <- expr
				  ; return $ (i,e)
				  })

instrChangeState :: TLAParser SH_Instr
instrChangeState = do { p <- getPosition
		      ; reserved "CHANGE" <|> reserved "CHANGES"
		      ; le <- commaSep viewOrExpr
		      ; return $ SH_I_ChangeState p le
		      }

instrChangeView :: TLAParser SH_Instr
instrChangeView = do { p <- getPosition
		     ; reserved "CHANGEVIEW"
		     ; i <- identifier
		     ; reservedOp "="
		     ; e <- expr
		     ; return $ SH_I_ChangeView p i e
		     }

instrTimerrestart :: TLAParser SH_Instr
instrTimerrestart = do { p <- getPosition
		       ; reserved "TIMERRESTART"
		       ; i <- identifier
		       ; reserved "DURATION"
		       ; d <- expr
		       ; return $ SH_I_Timerrestart p d i
		       }

instrTimercancel :: TLAParser SH_Instr
instrTimercancel = do { p <- getPosition
		      ; reserved "TIMERCANCEL"
		      ; i <- identifier
		      ; return $ SH_I_Timercancel p i
		      }

instrShutdown :: TLAParser SH_Instr
instrShutdown = do { p <- getPosition
		   ; reserved "SHUTDOWN"
		   ; return $ SH_I_Shutdown p
		   }

instrDrop :: TLAParser SH_Instr
instrDrop = do { p <- getPosition
	       ; reserved "DROP"
	       ; i <- identifier
	       ; return $ SH_I_Drop p i
	       }

instrReply :: TLAParser SH_Instr
instrReply = do { p <- getPosition
		; reserved "REPLY"
		; t <- identifier
		; msgrec <- option [] (do { m <- msgRecord
					  ; return m })
		; return $ SH_I_Reply p t msgrec
		}

instrAssert :: TLAParser SH_Instr
instrAssert = do { p <- getPosition
		 ; reserved "ASSERT"
		 ; (e,s,l) <- parens $ do { e <- expr
					  ; reservedOp ","
					  ; s <- stringLiteral
					  ; l <- option Nothing
					           (do { reservedOp ","
						       ; l <- commaSep expr
						       ; return $ Just l
						       })
					  ; return (e,s,l)
					  }
		 ; return $ SH_I_Assert p e s l 
		 }

-- only intended for code gen use
instrForeignChangeState :: TLAParser SH_Instr
instrForeignChangeState = do { p <- getPosition
			     ;     reserved "FOREIGNCHANGE" 
			       <|> reserved "FOREIGNCHANGES"
			     ; le <- commaSep viewOrExpr
			     ; reserved "IN"
			     ; role <- identifier
			     ; var <- option Nothing $
			                do { reserved "VAR"
					   ; e <- TLAParser.expression
					   ; return $ Just e
					   }
			     ; return $ SH_I_ForeignChangeState p role var le
			     }

instrLet :: TLAParser SH_Instr
instrLet = do { p <- getPosition
	      ; reserved "LET"
	      ; bindings <- commaSep letBinding
	      ; return $ SH_I_Let p bindings 
	      }

instrAwait :: TLAParser SH_Instr
instrAwait = do { p <- getPosition
		; handler <- msghandler False -- AWAIT 
		          -- <|> ...
		; return $ SH_I_Await p handler
		}

instrWhenBlockedIn :: TLAParser SH_Instr
instrWhenBlockedIn = do { p <- getPosition
			; reserved "WHILEIN"
			; reserved "{"
			; il <- instrList
			; reserved "}"
			; reserved "DO"
			; reserved "{"
			; handlers <- roleEl
			; reserved "}"
			; return $ SH_I_DoMeanwhile p il handlers
			}

instrBreak :: TLAParser SH_Instr
instrBreak = do { p <- getPosition
		; reserved "BREAK"
		; return $ SH_I_Break p 
		}

instrContinue :: TLAParser SH_Instr
instrContinue = do { p <- getPosition
		; reserved "CONTINUE"
		; return $ SH_I_Continue p 
		}

instrRewind :: TLAParser SH_Instr
instrRewind = do { p <- getPosition
		 ; reserved "REWIND"
		 ; id <- identifier
		 ; loc <- option Nothing $ do { reserved "TO"
					      ; loc <- identifier
					      ; return $ Just loc
					      }
		 ; st <- getState
		 ; let role = getRole st
		 ; let lowRole = map toLower role
		 ; return $ SH_I_Rewind p lowRole id loc
		 }

instrState :: TLAParser SH_Instr
instrState = do { p <- getPosition
		; s <- state
		; return $ SH_I_State p s
		}

letBinding :: TLAParser (String, SH_ExprWrapper)
letBinding = do { i <- identifier
		; reservedOp "="
		; e <- expr
		; return (i, e)
		}

ty :: TLAParser SH_Type
ty = do{ p <- getPosition
       ; t' <-  do{ ptype <- try paramtype 
		  ; return ptype
		  }
            <|> do{ t <- identifier
		  ; return $ SH_Ty_UserDef p t
		  }
       ; return t'
       }

paramtype :: TLAParser SH_Type
paramtype = do{ p <- getPosition
	      ; t <- choice [ do { reserved "Set"
				 ; reservedOp "<"
				 ; t <- ty
				 ; reservedOp ">"
				 ; return $ SH_Ty_SetOf p t
				 },
			      do { reserved "Seq"
				 ; reservedOp "<"
				 ; t <- ty
				 ; reservedOp ">"
				 ; return $ SH_Ty_SeqOf p t
				 },
			      do { reserved "Pair"
				 ; reservedOp "<"
				 ; a <- ty
				 ; reservedOp ","
				 ; b <- ty
				 ; reservedOp ">"
				 ; return $ SH_Ty_PairOf p a b
				 },
			      do { reserved "Nil"
				 ; reservedOp "<"
				 ; t <- ty
				 ; reservedOp ">"
				 ; return $ SH_Ty_UserDefOrNIL p t
				 }, 
			      do { reserved "Map"
				 ; reservedOp "<"
				 ; a <- ty
				 ; reservedOp ","
				 ; b <- ty
				 ; reservedOp ">"
				 ; return $ SH_Ty_Map p a b
				 },
			      do { reserved "Enum"
				 ; reservedOp "("
				 ; l <- sepBy1 identifier (reservedOp "|")
				 ; reservedOp ")"
				 ; return $ SH_Ty_Enum p l
				 }]
	      ; return t
	      }

-------------------------------------------------------------------------------
lexer = lexer0{P.reservedOp = rOp}
          where
            lexer0      = P.makeTokenParser shortdef
            resOp0      = P.reservedOp lexer0
            resOp1 name = do string name -- \in
                             notFollowedBy letter <?> ("end of " ++ show name)
            resOp2 name = do string name -- \ (set difference)
                             notFollowedBy (letter <|> char '/') <?> 
					       ("end of " ++ show name)
            resOp3 name = do string name -- \/ (or)
			     return ()
            ------
	    resOp4 name = do string name -- ~>
			     return ()
	    resOp5 name = do char '~' -- ~ (not)
			     return ()
            ------
            resOp6 name = do string name -- ==
                             notFollowedBy (char '=' {-??-} <|> char '>') <?> 
					       ("end of " ++ show name)
            resOp7 name = do string name -- =>
			     return ()
            resOp8 name = do string name -- =
                             notFollowedBy (char '=' <|> char '>') <?> 
					       ("end of " ++ show name)
            ------
            resOpLT0 name = do string name -- <>
                               notFollowedBy (char '>' {-??-} <|> char '=') <?> 
						 ("end of " ++ show name)
            resOpLT1 name = do string name -- <=
			       return ()
            resOpLT2 name = do string name -- <
                               notFollowedBy (char '=' <|> char '>') <?> 
						 ("end of " ++ show name)
            ------
            resOpAngular name = do string name -- []
                                   notFollowedBy (char ']') <?> 
						("end of " ++ show name)
            ------
            rOp name = lexeme $ try $
                          case name of
                            ('\\':cs@(_:_)) 
			        | cs == "/"      -> resOp3 name -- \/
				| all isAlpha cs -> resOp1 name -- \in, etc.
                            "\\" -> resOp2 name                 -- \ (set -)
			    ------
			    "~>" -> resOp4 name
			    "~"  -> resOp5 name
			    ------
			    "==" -> resOp6 name -- ==
			    "=>" -> resOp7 name -- =>
                            "="  -> resOp8 name -- =
			    ------
			    "<>" -> resOpLT0 name -- <>
			    "<=" -> resOpLT1 name -- <=
                            "<"  -> resOpLT2 name -- <
			    ------
			    "[]" -> resOpAngular name
			    ------
			    -- pre/postfix operator, special handling to cover
			    -- cases where operator is immediately followed by 
			    -- infix (e.g. cs'.x), or expression
			    -- (e.g. UNCHANGED<<cs>>)
			    "'" -> resOp1 name
			    "UNCHANGED" -> resOp1 name
			    "DOMAIN" -> resOp1 name
			    "SUBSET" -> resOp1 name
			    "UNION" -> resOp1 name
                            _ -> resOp0 name
	    isAlphaOrUnderscore c = isAlpha c || c == '_'
            lexeme p = do { x <- p; P.whiteSpace lexer0; return x }

shortdef = emptyDef {
  P.commentStart    = "(*"
, P.commentEnd      = "*)"
{------------- TLC EXTENSION ---------------}
, P.commentLine     = "//" -- use the familiar C syntax
, P.nestedComments  = True
{------------- TLC EXTENSION ---------------}
, P.identStart      = letter <|> char '_'
-- TLC EXTENSION, allow @ in identifiers for 'short' pc guards (when a@b ...)
, P.identLetter     = alphaNum <|> char '_' <|> char '@'
, P.opStart         = oneOf $ nub $ 
                        map (\s -> head s) $ P.reservedOpNames shortdef
, P.opLetter        = oneOf symbs
, P.reservedOpNames = [ -- prefix operators 
			"SUBSET"
		      , "DOMAIN"
		      , "UNION"
		      , "UNCHANGED"
		      , "INSTANCE"
		      , "[]"
		      , "<>"
		      , "~"
			-- postfix operators, REMEMBER to add to 'lexer' also!
		      , "'"
			-- infix operators
	              , "=", "#", ":>", "@@"
		      , "\\"
		      , "."
		      , ".."
		      , "+", "-", ">", "<", "<=", ">="
			-- general
	              , "=="
		      , "->"
		      , "~>"
		      , "|->"
		      , "=>"
		      , ":"
		      , "/="
		      , "\\in"
		      , "\\leq"
		      , "\\notin"
		      , "\\times" 
		      , "\\o", "\\circ"
		      , "\\subseteq", "\\cup", "\\cap"
		      , "\\X", "\\div"
		      , "\\/", "/\\"
		      , "<-"
{------------- TLC EXTENSION ---------------}
		      , "!", "!!" -- ATTENTION these are NOT TLC operators
{------------- TLC EXTENSION ---------------}
		      ]
, P.reservedNames   = [
{------------- TLC EXTENSION ---------------}
		      -- short specific
		        "PROTOCOL", "CONCERN"
		      , "INTERACTION", "TLA", "OVERRIDE"
		      , "ROLES", "STATE"
		        {- ,"VIEW" HACK FIXME, took out to allow for VIEW(R) -}
                        {- where VIEW is part of an expression              -}
		      , "VIEWS" 
		      , "SHARE", "AMONG"
                      , "TIMER", "EVERY"
		      , "WHEN", "WHILE", "HANDLE", "HANDLES", "END", "AWAIT"
		      , "EVENT", "ANY", "MSG", "MSGS", "TIMEOUT", "CRASH"
		      , "FROM", "MAJORITY", "ALL", "WHERE"
		      , "EXTEND"
		      , "TO", "TAG"
		      , "CHANGE", "CHANGES", "CHANGEVIEW"
		      , "FOREIGNCHANGE", "FOREIGNCHANGES"
		      , "SHUTDOWN", "DROP", "REPLY"
		      , "TIMERCANCEL" , "TIMERRESTART", "DURATION"
		      , "MAP", "SET", "ENUM", "BREAK", "REWIND"
		      , "WHILEIN", "DO"
		      , "DISPLAY", "SWIMLANE", "COLOR", "STYLE"
		      , "USING", "USE", "OF"
		      , "PERSISTENT"
		      , "DISABLE", "DISABLED"
{------------- TLC EXTENSION ---------------}
	              -- borrowed from TLA+
		      , "CONSTANT", "CONSTANTS"
	              , "ASSUME"
		      , "LET", "IN", "IF", "THEN", "ELSE", "CHOOSE",
			"EXCEPT", "!.", "CASE", "[]"
		      -- operator synonyms
		      , "\\A", "\\E"
		      , "TRUE", "FALSE"
		      -- CONFIG CFG FILE
		      , "SPECIFICATION"
		      , "INVARIANT", "INVARIANTS"
		      , "PROPERTY", "PROPERTIES"
		      , "SYMMETRY" {- ,"VIEW", see comment on VIEW above -}
		      ]
, P.caseSensitive   = True }
  where
    symbs = filter (not . isAlpha) . concat $ P.reservedOpNames shortdef

dot             = P.dot lexer    
parens          = P.parens lexer    
braces          = P.braces lexer    
squares         = P.squares lexer    
semiSep         = P.semiSep lexer  
semiSep1        = P.semiSep1 lexer    
commaSep        = P.commaSep lexer
commaSep1       = P.commaSep1 lexer
brackets        = P.brackets lexer
whiteSpace      = P.whiteSpace lexer    
symbol          = P.symbol lexer    
identifier      = P.identifier lexer    
reservedX       = P.reserved lexer    
reservedOp      = P.reservedOp lexer
integer         = P.integer lexer    
natural         = P.natural lexer    
charLiteral     = P.charLiteral lexer    
stringLiteral   = P.stringLiteral lexer

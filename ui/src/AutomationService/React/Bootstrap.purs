module AutomationService.React.Bootstrap
  ( accordion
  , accordionBody
  , accordionButton
  , accordionCollapse
  , accordionHeader
  , accordionHeader_
  , accordionItem
  , card
  , cardBody
  , cardFooter
  , cardGroup
  , cardHeader
  , cardImg
  , cardImgOverlay
  , cardLink
  , cardSubtitle
  , cardText
  , cardTitle
  , iconToggle
  )
 where

import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Data.Undefined.NoProblem (Opt)
import Elmish.React (ReactElement, createElement)
import Elmish.React.Import (ImportedReactComponent,
                            ImportedReactComponentConstructorWithContent)
import Elmish.HTML.Events (EventHandler, SyntheticEvent, handleEffect, stopPropagation)
import Elmish.HTML.Styled as H
import Foreign.Object (Object)
import Prelude (Unit, ($), (<<<), const, pure, unit)

-- Accordion

type AccordionProps =
  ( defaultActiveKey :: Opt Int )

type AccordionItemProps =
  ( eventKey :: Int )

type AccordionCollapseProps =
  ( eventKey :: Int )

accordion :: ImportedReactComponentConstructorWithContent AccordionProps
accordion = createElement accordion_

accordionItem :: ImportedReactComponentConstructorWithContent AccordionItemProps
accordionItem = createElement accordionItem_

accordionHeader :: ImportedReactComponentConstructorWithContent ()
accordionHeader = createElement accordionHeader_

accordionBody :: ImportedReactComponentConstructorWithContent ()
accordionBody = createElement accordionBody_

accordionButton :: ImportedReactComponentConstructorWithContent ()
accordionButton = createElement accordionButton_

accordionCollapse
  :: ImportedReactComponentConstructorWithContent AccordionCollapseProps
accordionCollapse = createElement accordionCollapse_

foreign import accordion_ :: ImportedReactComponent
foreign import accordionItem_ :: ImportedReactComponent
foreign import accordionHeader_ :: ImportedReactComponent
foreign import accordionBody_ :: ImportedReactComponent
foreign import accordionButton_ :: ImportedReactComponent
foreign import accordionCollapse_ :: ImportedReactComponent

foreign import useAccordionButton_
  :: forall a. Int -> EffectFn1 SyntheticEvent Unit -> EffectFn1 SyntheticEvent Unit

type IconClass = String

iconToggle
  :: forall r. { eventKey :: Int | r }
  -> IconClass
  -> ReactElement
iconToggle { eventKey } iconClass =
  H.div_ ""
  { -- onClick: handleEffect $ stopPropagation
    -- when I use this, the app crashes with "Invalid React Hook,"
    -- and I have no idea why this is, so leave this here for now as
    -- a TODO
    onClick: useAccordionButton_ eventKey $ mkEffectFn1 \_e -> pure unit
  } $
  H.i iconClass ""


-- Card

--
-- I originally added all of these because I thought maybe it was
-- somehow providing a context that me just using divs with card
-- classes was not, and maybe that was why useAccordionButton was
-- blowing everything up whenever I used it, but that was not the
-- case, and now it looks like I'm going to have to read some
-- javascript.
--

card :: ImportedReactComponentConstructorWithContent ()
card = createElement card_

cardBody :: ImportedReactComponentConstructorWithContent ()
cardBody = createElement cardBody_

cardFooter :: ImportedReactComponentConstructorWithContent ()
cardFooter = createElement cardFooter_

cardHeader :: ImportedReactComponentConstructorWithContent ()
cardHeader = createElement cardHeader_

cardImg :: ImportedReactComponentConstructorWithContent ()
cardImg = createElement cardImg_

cardImgOverlay :: ImportedReactComponentConstructorWithContent ()
cardImgOverlay = createElement cardImgOverlay_

cardLink :: ImportedReactComponentConstructorWithContent ()
cardLink = createElement cardLink_

cardSubtitle :: ImportedReactComponentConstructorWithContent ()
cardSubtitle = createElement cardSubtitle_

cardText :: ImportedReactComponentConstructorWithContent ()
cardText = createElement cardText_

cardTitle :: ImportedReactComponentConstructorWithContent ()
cardTitle = createElement cardTitle_

cardGroup :: ImportedReactComponentConstructorWithContent ()
cardGroup = createElement cardGroup_

foreign import card_ :: ImportedReactComponent
foreign import cardBody_ :: ImportedReactComponent
foreign import cardFooter_ :: ImportedReactComponent
foreign import cardHeader_ :: ImportedReactComponent
foreign import cardImg_ :: ImportedReactComponent
foreign import cardImgOverlay_ :: ImportedReactComponent
foreign import cardLink_ :: ImportedReactComponent
foreign import cardSubtitle_ :: ImportedReactComponent
foreign import cardText_ :: ImportedReactComponent
foreign import cardTitle_ :: ImportedReactComponent
foreign import cardGroup_ :: ImportedReactComponent

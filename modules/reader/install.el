
(use-package elfeed
  :general
  ("se" 'elfeed)

  (:keymaps 'elfeed-search-mode-map
   :states  'normal
   :prefix   nil

   "SPC" 'elfeed-control-panel/body)
  
  :init 
  (setq elfeed-search-filter "@1-month-ago +unread" ;; Default filter
        elfeed-feeds '(("https://bartoszmilewski.com/feed" FP)

                       ;; Emacs
                       ("http://irreal.org/blog/?feed=rss2" Emacs)
                       ("http://karl-voit.at/feeds/lazyblorg-all.atom_1.0.links-and-content.xml" Emacs)

                       ;; C++
                       ("https://www.fluentcpp.com/feed/" C++)
                       ("https://arne-mertz.de/feed/" C++)
                       ("http://www.modernescpp.com/index.php/component/jaggyblog/format=feed&type=rss" C++)
                       ("http://feeds.feedburner.com/codeandgraphics" C++)
                       ("https://herbsutter.com/feed/" C++)
                       ("https://www.meetingcpp.com/feed.xml" C++)
                       ("https://akrzemi1.wordpress.com/feed/" C++)
                       ("https://blogs.msdn.microsoft.com/vcblog/feed/" C++)
                       
                       ;; Rust
                       ("https://this-week-in-rust.org/rss.xml" Rust)

                       ;; GameDev
                       ("https://www.unrealengine.com/rss" GameDev UnrealEngine)
                       ("http://labs.domipheus.com/blog/feed/" GameDev)
                       ("http://msinilo.pl/blog/?feed=rss2" GameDev)
                       ("https://0fps.net/feed/" GameDev)
                       ("http://www.sebastiansylvan.com/index.xml" GameDev)
                       ("http://accidentallyquadratic.tumblr.com/rss" GameDev)
                       ("http://www.alexandre-pestana.com/feed/" GameDev)
                       ("http://algassert.com/feed.xml" GameDev)
                       ("http://deplinenoise.wordpress.com/feed/" GameDev)
                       ("http://akrzemi1.wordpress.com/feed/" GameDev)
                       ("http://c0de517e.blogspot.com/feeds/posts/default" GameDev)
                       ("https://anteru.net/rss.xml" GameDev)
                       ("http://aras-p.info/atom.xml" GameDev)
                       ("https://erkaman.github.io/rss.xml" GameDev)
                       ("http://voxelium.wordpress.com/feed/" GameDev)
                       ("http://blog.tobias-franke.eu/rss" GameDev)
                       ("http://www.enkisoftware.com/rss" GameDev)
                       ("http://bartwronski.com/feed/" GameDev)
                       ("http://bitbashing.io/feed.xml" GameDev)
                       ("http://bitsquid.blogspot.com/feeds/posts/default" GameDev)
                       ("http://www.binomial.info/blog?format=RSS" GameDev)
                       ("http://stephaniehurlburt.com/blog?format=RSS" GameDev)
                       ("http://graphicrants.blogspot.com/feeds/posts/default" GameDev)
                       ("http://briansharpe.wordpress.com/feed/" GameDev)
                       ("http://randomascii.wordpress.com/feed/" GameDev)
                       ("http://cppsecrets.blogspot.com/feeds/posts/default" GameDev)
                       ("http://mollyrocket.com/casey/stream_atom.rss" GameDev)
                       ("http://blog.xyzw.us/feeds/posts/default?alt=rss" GameDev)
                       ("http://casual-effects.blogspot.com/feeds/posts/default" GameDev)
                       ("http://cbloomrants.blogspot.com/feeds/posts/default" GameDev)
                       ("http://realtimecollisiondetection.net/blog/?feed=rss2" GameDev)
                       "http://www.thetenthplanet.de/feed"
                       "http://codecapsule.com/feed/"
                       "http://code4k.blogspot.com/feeds/posts/default"
                       "http://blog.dimitridiakopoulos.com/rss/"
                       "http://www.rorydriscoll.com/feed/"
                       "http://www.codersnotes.com/rss/"
                       "http://feeds.feedburner.com/codinghorror/"
                       "https://computingandrecording.wordpress.com/feed/"
                       "http://copypastepixel.blogspot.com/feeds/posts/default"
                       "http://blog.icare3d.org/feeds/posts/default"
                       "http://danluu.com/atom.xml"
                       "https://blog.forrestthewoods.com/feed"
                       "https://flashypixels.wordpress.com/feed/"
                       "http://blog.duangle.com/feeds/posts/default"
                       "http://blog.regehr.org/feed"
                       "https://emilypriceisright.com/feed/"
                       "http://www.epicshaders.com/feed/"
                       "http://fgiesen.wordpress.com/feed/"
                       "http://fabiensanglard.net/rss.xml"
                       "http://filmicworlds.com/feed.xml"
                       "http://eev.ee/feeds/atom.xml"
                       "https://fuzzyreflection.com/feed/"
                       "http://new.gafferongames.com/index.xml"
                       "http://www.petecollier.com/?feed=rss2"
                       "http://donw.io/index.xml"
                       "https://gpfault.net/rss.xml"
                       "http://www.decarpentier.nl/feed"
                       "http://ginsweater.com/blog/feed/"
                       "http://www.glowybits.com/index.xml"
                       "http://gpudissector.blogspot.com/feeds/posts/default"
                       "http://gpupro.blogspot.com/feeds/posts/default"
                       "http://gpuzen.blogspot.com/feeds/posts/default?alt=rss"
                       "http://imdoingitwrong.wordpress.com/feed/"
                       "http://castano.ludicon.com/blog/feed/"
                       "https://imgtec.com/feed/"
                       "http://industrialarithmetic.blogspot.com/feeds/posts/default"
                       "http://ir-ltd.net/feed/"
                       "http://jamesdolan.blogspot.com/feeds/posts/default"
                       "http://playtechs.blogspot.com/feeds/posts/default"
                       "https://jendrikillner.bitbucket.io/index.xml"
                       "http://www.forceflow.be/feed/"
                       "http://joeduffyblog.com/feed.xml"
                       "http://repi.blogspot.com/feeds/posts/default"
                       "http://john-ahlgren.blogspot.com/feeds/posts/default"
                       "http://www.johncalsbeek.com/feed.xml"
                       "http://johnwhite3d.blogspot.com/feeds/posts/default"
                       "http://olickspulpit.blogspot.com/feeds/posts/default"
                       "http://www.jonolick.com/2/feed"
                       "http://joostdevblog.blogspot.com/feeds/posts/default"
                       "http://www.iryoku.com/feed"
                       "http://jvns.ca/atom.xml"
                       "http://daugaard.org/blog/?feed=rss2"
                       "https://kate.io/feed.xml"
                       "http://kosmokleaner.wordpress.com/feed/"
                       "http://kosmonautblog.wordpress.com/feed/"
                       "http://interplayoflight.wordpress.com/feed/"
                       "http://kristerw.blogspot.com/feeds/posts/default"
                       "http://knarkowicz.wordpress.com/feed/"
                       "http://leighalexander.net/feed/"
                       "http://feeds.lia-sae.net/main.rss.xml"
                       "http://lousodrome.net/blog/light/feed/"
                       "http://themaister.net/blog/feed/"
                       "http://marc-b-reynolds.github.io/feed.xml"
                       "http://blogs.msdn.com/marcelolr/rss.xml"
                       "http://blog.marmakoide.org/?feed=rss2"
                       "http://mynameismjp.wordpress.com/feed/"
                       "http://directtovideo.wordpress.com/feed/"
                       "http://deanoc.com/rss.xml"
                       "https://tuxedolabs.blogspot.com/feeds/posts/default?alt=rss"
                       "http://www.dimension3.sk/feed/"
                       "http://blog.mmacklin.com/feed/"
                       "http://allenchou.net/feed/"
                       "http://molecularmusings.wordpress.com/feed/"
                       "http://mmikkelsen3d.blogspot.com/feeds/posts/default"
                       "http://reedbeta.com/feed/"
                       "https://nlguillemot.wordpress.com/feed/"
                       "http://www.openglsuperbible.com/feed/"
                       "http://ourmachinery.com/index.xml"
                       "http://outerra.blogspot.com/feeds/posts/default"
                       "http://pzurita.wordpress.com/feed/"
                       "http://psgraphics.blogspot.com/feeds/posts/default"
                       "http://petersikachev.blogspot.com/feeds/posts/default?alt=rss"
                       "http://pixeljetstream.blogspot.com/feeds/posts/default"
                       "http://pointersgonewild.com/feed/"
                       "http://preshing.com/feed"
                       "http://prog21.dadgum.com/atom.xml"
                       "http://ventspace.wordpress.com/feed/"
                       "http://blog.shivoa.net/feeds/posts/default"
                       "http://chainedchaos31.tumblr.com/rss"
                       "http://www.randygaul.net/feed/"
                       "http://raytracey.blogspot.com/feeds/posts/default"
                       "http://www.realtimerendering.com/blog/feed/"
                       "http://realtimevoxels.blogspot.com/feeds/posts/default"
                       "http://richg42.blogspot.com/feeds/posts/default"
                       "http://www.curious-creature.org/feed/"
                       "http://msm.grumpybumpers.com/?feed=rss2"
                       "http://seblagarde.wordpress.com/feed/"
                       "http://www.palgorithm.co.uk/feed/"
                       "https://sandervanrossen.blogspot.com/feeds/posts/default"
                       "http://vec3.ca/feed/"
                       "http://simonschreibt.blogspot.com/feeds/posts/default"
                       "http://sjbrown.co.uk/index.xml"
                       "http://www.sophiehoulden.com/feed/"
                       "http://blog.selfshadow.com/feed/"
                       "http://blog.stevemcauley.com/feed/"
                       "http://steve-yegge.blogspot.com/feeds/posts/default"
                       "https://medium.com/feed/@Aprilw"
                       "https://medium.com/feed/@bgolus"
                       "https://medium.com/feed/@Esquiring"
                       "https://medium.com/feed/@steve.yegge"
                       "http://blog.hvidtfeldts.net/index.php/feed/"
                       "http://evincarofautumn.blogspot.com/feeds/posts/default"
                       "http://blog.demofox.org/feed/"
                       "http://kylehalladay.com/atom.xml"
                       "http://www.joshbarczak.com/blog/?feed=rss2"
                       "http://david.fancyfishgames.com/feeds/posts/default"
                       "http://blogs.msdn.com/oldnewthing/rss.xml"
                       "http://blogs.msdn.com/b/visualstudio/rss.aspx"
                       "http://the-witness.net/news/feed/"
                       "https://timothylottes.github.io/index.rss"
                       "http://grantland.com/contributors/tom-bissell/feed/"
                       "http://tomforsyth1000.github.io/blog.wiki.xml"
                       "http://tomhammersley.blogspot.com/feeds/posts/default"
                       "http://seven-degrees-of-freedom.blogspot.com/feeds/posts/default"
                       "http://sonnati.wordpress.com/feed/"
                       "https://blogs.msdn.microsoft.com/wsl/feed/"
                       "http://diaryofagraphicsprogrammer.blogspot.com/feeds/posts/default"
                       "http://xoofx.com/feed.xml"
                       "http://kayru.org/feed.xml"
                       "http://zrusin.blogspot.com/feeds/posts/default"
                       "http://zeuxcg.org/feed/"
                       "https://colinbarrebrisebois.com/feed/"
                       "http://4gravitons.wordpress.com/feed/"
                       "http://backreaction.blogspot.com/feeds/posts/default"
                       "http://cqgplus.com/feed/"
                       "http://www.science20.com/quantum_diaries_survivor/feed"
                       "https://ensnaredinvacuum.wordpress.com/feed/"
                       "http://blog.jessriedel.com/feed/"
                       "http://gravityandlevity.wordpress.com/feed/"
                       "http://feeds.feedburner.com/MattLeifer"
                       "http://www.math.columbia.edu/~woit/wordpress/?feed=rss2"
                       "http://profmattstrassler.com/feed/"
                       "https://api.quantamagazine.org/feed/"
                       "http://scottaaronson.com/blog/?feed=rss2"
                       "http://www.preposterousuniverse.com/blog/feed/"
                       "http://scienceblogs.com/startswithabang/index.xml"
                       "http://glenmartin.wordpress.com/feed/")
        
        elfeed-db-directory "~/Dropbox/Приложения/elfeeddb"
        
        elfeed-mac-connections 10
        url-queue-timeout 30)

  (defhydra elfeed-control-panel ()
    "elfeed"
    ("u" elfeed-update "update" :color blue)
    ("q" nil "quit" :color blue))
  
  :config
  (with-eval-after-load 'evil-collection
    (add-to-list 'evil-collection-mode-list 'elfeed)
    (evil-collection-init)))

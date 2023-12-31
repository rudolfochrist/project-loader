(require 'asdf)
(asdf:initialize-source-registry
 '(:source-registry
   (:directory (:here))
   (:tree (:here "systems/"))
   (:tree (:here "vendor/"))
   :inherit-configuration))

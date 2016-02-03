module HGE2D.Instances where

import HGE2D.Datas
import HGE2D.Classes

instance HasBoundingBox BoundingBox where
    getBB bb = bb

instance HasBoundingBox RigidBody where
    getBB rb = rigidBB rb

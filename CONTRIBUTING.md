# Contributing to Silvi

The most valuable thing that can be added to Silvi is more datatypes, along with documentation for how those datatypes present themselves within logs. The general outline of how to add these is below.

<ol>
  <li>In Silvi.Types, write a datetype with a sufficiently descriptive name, and an explanation of what that type is supposed to represent.</li>
  <li>In Silvi.Types (same module), add that datatype to Field, Value, SingField, and write its Reify instance. You can see how this is done within that module, it is very simple.</li>
  <li>In Silvi.Random, write a randomTYPENAME :: Gen TYPENAME function, these are all done in Applicative style and are very easy to write.</li>
  <li>In Silvi.Random.randLog, add the randomTYPENAME function in the same way that the others are written, this is again easily done from seeing how it is already done in randLog.</li>
  <li>In tests/TestAll.hs, add 'FieldTYPENAME to AllTypesLog, then run `cabal test` to make sure that Silvi can actually generate that type.</li>
</ol>

Don't forget that documentation for the datatypes is very important; If you cannot explain provide documentation for the type or <i>at the very least</i> explain the importance of the type, then it will take longer for a change to get merged.

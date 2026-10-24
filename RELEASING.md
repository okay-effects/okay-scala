# Releasing

Releases are a Maven repository on GitHub Pages: the `gh-pages` branch, its `maven/` directory, served at
https://okay-effects.github.io/okay-scala/maven. No account and no token is needed to use it, only a resolver.

## Using a release

```scala-sbt
resolvers += "okay-scala" at "https://okay-effects.github.io/okay-scala/maven"

libraryDependencies += "io.github.okay-effects" %% "okay-scala" % "0.1.1"
```

`%%%` for Scala.js and Scala Native. The library is built on Scala 3.3 LTS, so any Scala 3 from 3.3 on can use it.

## Making one

1. On `master`, everything green, on every platform and on both Scala 3s:

   ```sh
   sbt +coreJVM/test
   sbt -Dplatforms=jvm,js,native coreJS/test coreNative/test
   ```

2. The version: `ThisBuild / version` in `build.sbt` without `-SNAPSHOT`, committed, tagged (`v0.1.0`).

3. The repository written into `target/pages-maven/`, for all three platforms — WITHOUT `+`: the `_3` artifacts are
   Scala 3.3's, and a `+publish` would overwrite them with the newest Scala's, which 3.3 cannot read:

   ```sh
   rm -rf target/pages-maven
   sbt -Dplatforms=jvm,js,native coreJVM/publish coreJS/publish coreNative/publish
   ```

4. Onto `gh-pages`, under `maven/`, beside the releases already there:

   ```sh
   git worktree add /tmp/okay-scala-pages gh-pages    # the first time: git worktree add --orphan -b gh-pages /tmp/okay-scala-pages
   mkdir -p /tmp/okay-scala-pages/maven
   rsync -a target/pages-maven/ /tmp/okay-scala-pages/maven/
   touch /tmp/okay-scala-pages/.nojekyll
   git -C /tmp/okay-scala-pages add -A
   git -C /tmp/okay-scala-pages commit -m "release 0.1.0"
   git -C /tmp/okay-scala-pages push origin gh-pages
   git push origin v0.1.0
   ```

   The first time, GitHub Pages is turned on for the branch `gh-pages`.

5. The version back to the next `-SNAPSHOT`.

A release is never rewritten: a version published is what every build that resolved it has.

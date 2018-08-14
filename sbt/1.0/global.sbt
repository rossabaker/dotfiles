import com.typesafe.sbt.pgp.PgpKeys.gpgCommand

credentials += Credentials(Path.userHome / ".sbt" / ".credentials")

useGpg in Global := true
gpgCommand in Global := "gpg2"

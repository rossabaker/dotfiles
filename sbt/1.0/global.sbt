import com.typesafe.sbt.pgp.PgpKeys.gpgCommand

useGpg in Global := true
gpgCommand in Global := "gpg2"

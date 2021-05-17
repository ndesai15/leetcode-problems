package com.rockthejvm.strings

import scala.annotation.tailrec

/**
  * @author ndesai on 2021-05-03
  *
  */


object ValidEmailAddress {

  def numUniqueEmails(emails: List[String]): Int = {
    @tailrec
    def uniqueEmailTailRec(remaining: List[String], result: Set[String]): Int = {
      if (remaining.isEmpty) result.size
      else {
        val splitted = remaining.head.split("@").toList
        val localName = splitted.head.replace(".", "").split('+').head
        val domainName = splitted.last
        uniqueEmailTailRec(remaining.tail, result + s"$localName@$domainName")
      }
    }

    if (emails.isEmpty) 0
    else uniqueEmailTailRec(emails, Set())
  }

  def main(args: Array[String]): Unit = {
    println(numUniqueEmails(List("test.email+alex@leetcode.com", "test.e.mail+bob.cathy@leetcode.com", "testemail+david@lee.tcode.com")))
    println(numUniqueEmails(List("a@leetcode.com", "b@leetcode.com", "c@leetcode.com")))

    println(numUniqueEmails(List("fg.r.u.uzj+o.pw@kziczvh.com",
      "r.cyo.g+d.h+b.ja@tgsg.z.com",
      "fg.r.u.uzj+o.f.d@kziczvh.com",
      "r.cyo.g+ng.r.iq@tgsg.z.com",
      "fg.r.u.uzj+lp.k@kziczvh.com",
      "r.cyo.g+n.h.e+n.g@tgsg.z.com",
      "fg.r.u.uzj+k+p.j@kziczvh.com",
      "fg.r.u.uzj+w.y+b@kziczvh.com",
      "r.cyo.g+x+d.c+f.t@tgsg.z.com",
      "r.cyo.g+x+t.y.l.i@tgsg.z.com",
      "r.cyo.g+brxxi@tgsg.z.com",
      "r.cyo.g+z+dr.k.u@tgsg.z.com",
      "r.cyo.g+d+l.c.n+g@tgsg.z.com",
      "fg.r.u.uzj+vq.o@kziczvh.com",
      "fg.r.u.uzj+uzq@kziczvh.com",
      "fg.r.u.uzj+mvz@kziczvh.com",
      "fg.r.u.uzj+taj@kziczvh.com",
      "fg.r.u.uzj+fek@kziczvh.com")))
  }
}

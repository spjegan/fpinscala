package com.fpinscala.chap8

/**
 * Created by Jegan on 7/20/2015.
 */
trait Prop {

  def check: Boolean

  def &&(p: Prop): Prop = new Prop {
    override def check: Boolean = Prop.this.check && p.check
  }
}
if (!strcmp ("arg.c", insn_op)) {
  ASSERT_LEN (1);
  ASSERT_SYM (insn_1);
  def_arg (arg_hash, insn_1, jit_arg_c ());
} else if (!strcmp ("arg.i", insn_op)) {
  ASSERT_LEN (1);
  ASSERT_SYM (insn_1);
  def_arg (arg_hash, insn_1, jit_arg_i ());
} else if (!strcmp ("arg.l", insn_op)) {
  ASSERT_LEN (1);
  ASSERT_SYM (insn_1);
  def_arg (arg_hash, insn_1, jit_arg_l ());
} else if (!strcmp ("arg", insn_op)) {
  ASSERT_LEN (1);
  ASSERT_SYM (insn_1);
  def_arg (arg_hash, insn_1, jit_arg_l ());
} else if (!strcmp ("arg.p", insn_op)) {
  ASSERT_LEN (1);
  ASSERT_SYM (insn_1);
  def_arg (arg_hash, insn_1, jit_arg_p ());
} else if (!strcmp ("arg.s", insn_op)) {
  ASSERT_LEN (1);
  ASSERT_SYM (insn_1);
  def_arg (arg_hash, insn_1, jit_arg_s ());
} else if (!strcmp ("arg.uc", insn_op)) {
  ASSERT_LEN (1);
  ASSERT_SYM (insn_1);
  def_arg (arg_hash, insn_1, jit_arg_uc ());
} else if (!strcmp ("arg.ui", insn_op)) {
  ASSERT_LEN (1);
  ASSERT_SYM (insn_1);
  def_arg (arg_hash, insn_1, jit_arg_ui ());
} else if (!strcmp ("arg.ul", insn_op)) {
  ASSERT_LEN (1);
  ASSERT_SYM (insn_1);
  def_arg (arg_hash, insn_1, jit_arg_ul ());
} else if (!strcmp ("arg.us", insn_op)) {
  ASSERT_LEN (1);
  ASSERT_SYM (insn_1);
  def_arg (arg_hash, insn_1, jit_arg_us ());
} else if (!strcmp ("add.i", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_addr_i (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_addi_i (d, s1, s2);
  }
} else if (!strcmp ("addx.i", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_addxr_i (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_addxi_i (d, s1, s2);
  }
} else if (!strcmp ("and.i", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_andr_i (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_andi_i (d, s1, s2);
  }
} else if (!strcmp ("beq.i", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_beqr_i (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_beqi_i (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("beq.i", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_beqr_i (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_beqi_i (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bge.i", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bger_i (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bgei_i (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bge.ui", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bger_ui (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bgei_ui (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bge.i", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bger_i (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bgei_i (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bge.ui", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bger_ui (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bgei_ui (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bgt.i", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bgtr_i (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bgti_i (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bgt.ui", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bgtr_ui (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bgti_ui (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bgt.i", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bgtr_i (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bgti_i (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bgt.ui", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bgtr_ui (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bgti_ui (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("ble.i", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bler_i (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_blei_i (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("ble.ui", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bler_ui (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_blei_ui (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("ble.i", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bler_i (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_blei_i (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("ble.ui", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bler_ui (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_blei_ui (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("blt.i", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bltr_i (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_blti_i (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("blt.ui", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bltr_ui (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_blti_ui (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("blt.i", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bltr_i (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_blti_i (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("blt.ui", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bltr_ui (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_blti_ui (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("boadd.i", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_boaddr_i (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_boaddi_i (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("boadd.ui", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_boaddr_ui (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_boaddi_ui (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("boadd.i", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_boaddr_i (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_boaddi_i (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("boadd.ui", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_boaddr_ui (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_boaddi_ui (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bosub.i", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bosubr_i (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bosubi_i (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bosub.ui", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bosubr_ui (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bosubi_ui (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bosub.i", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bosubr_i (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bosubi_i (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bosub.ui", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bosubr_ui (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bosubi_ui (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bmc.i", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bmcr_i (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bmci_i (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bmc.i", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bmcr_i (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bmci_i (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bms.i", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bmsr_i (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bmsi_i (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bms.i", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bmsr_i (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bmsi_i (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bne.i", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bner_i (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bnei_i (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bne.i", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bner_i (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bnei_i (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("calli", insn_op)) {
  unsigned long t;
  ASSERT_LEN (1);
  t = AS_INT (insn_1);
  jit_calli (t);
} else if (!strcmp ("div.i", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_divr_i (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_divi_i (d, s1, s2);
  }
} else if (!strcmp ("div.ui", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_divr_ui (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_divi_ui (d, s1, s2);
  }
} else if (!strcmp ("eq.i", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_eqr_i (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_eqi_i (d, s1, s2);
  }
} else if (!strcmp ("ge.i", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ger_i (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_gei_i (d, s1, s2);
  }
} else if (!strcmp ("ge.ui", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ger_ui (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_gei_ui (d, s1, s2);
  }
} else if (!strcmp ("gt.i", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_gtr_i (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_gti_i (d, s1, s2);
  }
} else if (!strcmp ("gt.ui", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_gtr_ui (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_gti_ui (d, s1, s2);
  }
} else if (!strcmp ("hmul.i", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_hmulr_i (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_hmuli_i (d, s1, s2);
  }
} else if (!strcmp ("hmul.ui", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_hmulr_ui (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_hmuli_ui (d, s1, s2);
  }
} else if (!strcmp ("jmp", insn_op)) {
  unsigned long t;
  ASSERT_LEN (1);
  if (IS_REG (insn_1)) {
    t = AS_REG (insn_1);
    jit_jmpr (t);
  } else {
    t = AS_INT (insn_1);
    jit_jmpi (t);
  }
} else if (!strcmp ("jmp", insn_op)) {
  unsigned long t;
  ASSERT_LEN (1);
  if (IS_REG (insn_1)) {
    t = AS_REG (insn_1);
    jit_jmpr (t);
  } else {
    t = AS_INT (insn_1);
    jit_jmpi (t);
  }
} else if (!strcmp ("ldx.c", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ldxr_c (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_ldxi_c (d, s1, s2);
  }
} else if (!strcmp ("ldx.i", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ldxr_i (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_ldxi_i (d, s1, s2);
  }
} else if (!strcmp ("ldx.s", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ldxr_s (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_ldxi_s (d, s1, s2);
  }
} else if (!strcmp ("ldx.uc", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ldxr_uc (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_ldxi_uc (d, s1, s2);
  }
} else if (!strcmp ("ldx.us", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ldxr_us (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_ldxi_us (d, s1, s2);
  }
} else if (!strcmp ("le.i", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ler_i (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_lei_i (d, s1, s2);
  }
} else if (!strcmp ("le.ui", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ler_ui (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_lei_ui (d, s1, s2);
  }
} else if (!strcmp ("lsh.i", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_lshr_i (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_lshi_i (d, s1, s2);
  }
} else if (!strcmp ("lt.i", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ltr_i (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_lti_i (d, s1, s2);
  }
} else if (!strcmp ("lt.ui", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ltr_ui (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_lti_ui (d, s1, s2);
  }
} else if (!strcmp ("mod.i", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_modr_i (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_modi_i (d, s1, s2);
  }
} else if (!strcmp ("mod.ui", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_modr_ui (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_modi_ui (d, s1, s2);
  }
} else if (!strcmp ("mov.i", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  d = AS_REG (insn_1);
  if (IS_REG (insn_2)) {
    s1 = AS_REG (insn_2);
    jit_movr_i (d, s1);
  } else {
    s1 = AS_INT (insn_2);
    jit_movi_i (d, s1);
  }
} else if (!strcmp ("mul.i", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_mulr_i (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_muli_i (d, s1, s2);
  }
} else if (!strcmp ("mul.ui", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_mulr_ui (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_muli_ui (d, s1, s2);
  }
} else if (!strcmp ("ne.i", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ner_i (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_nei_i (d, s1, s2);
  }
} else if (!strcmp ("nop", insn_op)) {
  ASSERT_LEN (0);
  jit_nop ();
} else if (!strcmp ("or.i", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_orr_i (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_ori_i (d, s1, s2);
  }
} else if (!strcmp ("pop.i", insn_op)) {
  int r;
  ASSERT_LEN (1);
  r = AS_REG (insn_1);
  jit_pop_i (r);
} else if (!strcmp ("prepare", insn_op)) {
  int i;
  ASSERT_LEN (1);
  i = AS_INT (insn_1);
  jit_prepare (i);
} else if (!strcmp ("push.i", insn_op)) {
  int r;
  ASSERT_LEN (1);
  r = AS_REG (insn_1);
  jit_push_i (r);
} else if (!strcmp ("pusharg.i", insn_op)) {
  int r;
  ASSERT_LEN (1);
  r = AS_REG (insn_1);
  jit_pusharg_i (r);
} else if (!strcmp ("ret", insn_op)) {
  ASSERT_LEN (0);
  jit_ret ();
} else if (!strcmp ("retval.i", insn_op)) {
  int r;
  ASSERT_LEN (1);
  r = AS_REG (insn_1);
  jit_retval_i (r);
} else if (!strcmp ("retval", insn_op)) {
  int r;
  ASSERT_LEN (1);
  r = AS_REG (insn_1);
  jit_retval_i (r);
} else if (!strcmp ("rsh.i", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_rshr_i (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_rshi_i (d, s1, s2);
  }
} else if (!strcmp ("rsh.ui", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_rshr_ui (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_rshi_ui (d, s1, s2);
  }
} else if (!strcmp ("stx.c", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  s1 = AS_REG (insn_2);
  s2 = AS_REG (insn_3);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_stxr_c (d, s1, s2);
  } else {
    d = AS_INT (insn_1);
    jit_stxi_c (d, s1, s2);
  }
} else if (!strcmp ("stx.i", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  s1 = AS_REG (insn_2);
  s2 = AS_REG (insn_3);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_stxr_i (d, s1, s2);
  } else {
    d = AS_INT (insn_1);
    jit_stxi_i (d, s1, s2);
  }
} else if (!strcmp ("stx.s", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  s1 = AS_REG (insn_2);
  s2 = AS_REG (insn_3);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_stxr_s (d, s1, s2);
  } else {
    d = AS_INT (insn_1);
    jit_stxi_s (d, s1, s2);
  }
} else if (!strcmp ("stx.c", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  s1 = AS_REG (insn_2);
  s2 = AS_REG (insn_3);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_stxr_c (d, s1, s2);
  } else {
    d = AS_INT (insn_1);
    jit_stxi_c (d, s1, s2);
  }
} else if (!strcmp ("stx.i", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  s1 = AS_REG (insn_2);
  s2 = AS_REG (insn_3);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_stxr_i (d, s1, s2);
  } else {
    d = AS_INT (insn_1);
    jit_stxi_i (d, s1, s2);
  }
} else if (!strcmp ("stx.s", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  s1 = AS_REG (insn_2);
  s2 = AS_REG (insn_3);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_stxr_s (d, s1, s2);
  } else {
    d = AS_INT (insn_1);
    jit_stxi_s (d, s1, s2);
  }
} else if (!strcmp ("sub.i", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_subr_i (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_subi_i (d, s1, s2);
  }
} else if (!strcmp ("subx.i", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_subxr_i (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_subxi_i (d, s1, s2);
  }
} else if (!strcmp ("xor.i", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_xorr_i (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_xori_i (d, s1, s2);
  }
} else if (!strcmp ("neg.i", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  d = AS_REG (insn_1);
  if (IS_REG (insn_2)) {
    s1 = AS_REG (insn_2);
    jit_negr_i (d, s1);
  } else {
    s1 = AS_INT (insn_2);
    jit_negi_i (d, s1);
  }
} else if (!strcmp ("neg.l", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  d = AS_REG (insn_1);
  if (IS_REG (insn_2)) {
    s1 = AS_REG (insn_2);
    jit_negr_l (d, s1);
  } else {
    s1 = AS_INT (insn_2);
    jit_negi_l (d, s1);
  }
} else if (!strcmp ("neg", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  d = AS_REG (insn_1);
  if (IS_REG (insn_2)) {
    s1 = AS_REG (insn_2);
    jit_negr_l (d, s1);
  } else {
    s1 = AS_INT (insn_2);
    jit_negi_l (d, s1);
  }
} else if (!strcmp ("prolog", insn_op)) {
  int i;
  ASSERT_LEN (1);
  i = AS_INT (insn_1);
  jit_prolog (i);
} else if (!strcmp ("finish", insn_op)) {
  unsigned long t;
  ASSERT_LEN (1);
  t = AS_INT (insn_1);
  jit_finish (t);
} else if (!strcmp ("leaf", insn_op)) {
  int i;
  ASSERT_LEN (1);
  i = AS_INT (insn_1);
  jit_leaf (i);
} else if (!strcmp ("getarg.c", insn_op)) {
  int r, a;
  ASSERT_LEN (2);
  r = AS_REG (insn_1);
  ASSERT_SYM (insn_2);
  a = get_arg (arg_hash, insn_2);
  jit_getarg_c (r, a);
} else if (!strcmp ("getarg.i", insn_op)) {
  int r, a;
  ASSERT_LEN (2);
  r = AS_REG (insn_1);
  ASSERT_SYM (insn_2);
  a = get_arg (arg_hash, insn_2);
  jit_getarg_i (r, a);
} else if (!strcmp ("getarg.l", insn_op)) {
  int r, a;
  ASSERT_LEN (2);
  r = AS_REG (insn_1);
  ASSERT_SYM (insn_2);
  a = get_arg (arg_hash, insn_2);
  jit_getarg_l (r, a);
} else if (!strcmp ("getarg", insn_op)) {
  int r, a;
  ASSERT_LEN (2);
  r = AS_REG (insn_1);
  ASSERT_SYM (insn_2);
  a = get_arg (arg_hash, insn_2);
  jit_getarg_l (r, a);
} else if (!strcmp ("getarg.p", insn_op)) {
  int r, a;
  ASSERT_LEN (2);
  r = AS_REG (insn_1);
  ASSERT_SYM (insn_2);
  a = get_arg (arg_hash, insn_2);
  jit_getarg_p (r, a);
} else if (!strcmp ("getarg.s", insn_op)) {
  int r, a;
  ASSERT_LEN (2);
  r = AS_REG (insn_1);
  ASSERT_SYM (insn_2);
  a = get_arg (arg_hash, insn_2);
  jit_getarg_s (r, a);
} else if (!strcmp ("getarg.uc", insn_op)) {
  int r, a;
  ASSERT_LEN (2);
  r = AS_REG (insn_1);
  ASSERT_SYM (insn_2);
  a = get_arg (arg_hash, insn_2);
  jit_getarg_uc (r, a);
} else if (!strcmp ("getarg.ui", insn_op)) {
  int r, a;
  ASSERT_LEN (2);
  r = AS_REG (insn_1);
  ASSERT_SYM (insn_2);
  a = get_arg (arg_hash, insn_2);
  jit_getarg_ui (r, a);
} else if (!strcmp ("getarg.ul", insn_op)) {
  int r, a;
  ASSERT_LEN (2);
  r = AS_REG (insn_1);
  ASSERT_SYM (insn_2);
  a = get_arg (arg_hash, insn_2);
  jit_getarg_ul (r, a);
} else if (!strcmp ("getarg.us", insn_op)) {
  int r, a;
  ASSERT_LEN (2);
  r = AS_REG (insn_1);
  ASSERT_SYM (insn_2);
  a = get_arg (arg_hash, insn_2);
  jit_getarg_us (r, a);
} else if (!strcmp ("ntoh.ui", insn_op)) {
  int r1, r2;
  ASSERT_LEN (2);
  r1 = AS_REG (insn_1);
  r2 = AS_REG (insn_2);
  jit_ntoh_ui (r1, r2);
} else if (!strcmp ("ntoh.us", insn_op)) {
  int r1, r2;
  ASSERT_LEN (2);
  r1 = AS_REG (insn_1);
  r2 = AS_REG (insn_2);
  jit_ntoh_us (r1, r2);
} else if (!strcmp ("ld.c", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  d = AS_REG (insn_1);
  if (IS_REG (insn_2)) {
    s1 = AS_REG (insn_2);
    jit_ldr_c (d, s1);
  } else {
    s1 = AS_INT (insn_2);
    jit_ldi_c (d, s1);
  }
} else if (!strcmp ("ld.i", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  d = AS_REG (insn_1);
  if (IS_REG (insn_2)) {
    s1 = AS_REG (insn_2);
    jit_ldr_i (d, s1);
  } else {
    s1 = AS_INT (insn_2);
    jit_ldi_i (d, s1);
  }
} else if (!strcmp ("ld.s", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  d = AS_REG (insn_1);
  if (IS_REG (insn_2)) {
    s1 = AS_REG (insn_2);
    jit_ldr_s (d, s1);
  } else {
    s1 = AS_INT (insn_2);
    jit_ldi_s (d, s1);
  }
} else if (!strcmp ("ld.uc", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  d = AS_REG (insn_1);
  if (IS_REG (insn_2)) {
    s1 = AS_REG (insn_2);
    jit_ldr_uc (d, s1);
  } else {
    s1 = AS_INT (insn_2);
    jit_ldi_uc (d, s1);
  }
} else if (!strcmp ("ld.ui", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  d = AS_REG (insn_1);
  if (IS_REG (insn_2)) {
    s1 = AS_REG (insn_2);
    jit_ldr_ui (d, s1);
  } else {
    s1 = AS_INT (insn_2);
    jit_ldi_ui (d, s1);
  }
} else if (!strcmp ("ld.ul", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  d = AS_REG (insn_1);
  if (IS_REG (insn_2)) {
    s1 = AS_REG (insn_2);
    jit_ldr_ul (d, s1);
  } else {
    s1 = AS_INT (insn_2);
    jit_ldi_ul (d, s1);
  }
} else if (!strcmp ("ld.us", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  d = AS_REG (insn_1);
  if (IS_REG (insn_2)) {
    s1 = AS_REG (insn_2);
    jit_ldr_us (d, s1);
  } else {
    s1 = AS_INT (insn_2);
    jit_ldi_us (d, s1);
  }
} else if (!strcmp ("st.c", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_str_c (d, s1);
  } else {
    d = AS_INT (insn_1);
    jit_sti_c (d, s1);
  }
} else if (!strcmp ("st.i", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_str_i (d, s1);
  } else {
    d = AS_INT (insn_1);
    jit_sti_i (d, s1);
  }
} else if (!strcmp ("st.s", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_str_s (d, s1);
  } else {
    d = AS_INT (insn_1);
    jit_sti_s (d, s1);
  }
} else if (!strcmp ("st.c", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_str_c (d, s1);
  } else {
    d = AS_INT (insn_1);
    jit_sti_c (d, s1);
  }
} else if (!strcmp ("st.i", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_str_i (d, s1);
  } else {
    d = AS_INT (insn_1);
    jit_sti_i (d, s1);
  }
} else if (!strcmp ("st.s", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_str_s (d, s1);
  } else {
    d = AS_INT (insn_1);
    jit_sti_s (d, s1);
  }
} else if (!strcmp ("add.p", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_addr_p (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_addi_p (d, s1, s2);
  }
} else if (!strcmp ("add.ui", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_addr_ui (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_addi_ui (d, s1, s2);
  }
} else if (!strcmp ("add.ul", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_addr_ul (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_addi_ul (d, s1, s2);
  }
} else if (!strcmp ("and.ui", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_andr_ui (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_andi_ui (d, s1, s2);
  }
} else if (!strcmp ("and.ul", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_andr_ul (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_andi_ul (d, s1, s2);
  }
} else if (!strcmp ("beq.p", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_beqr_p (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_beqi_p (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("beq.ui", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_beqr_ui (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_beqi_ui (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("beq.ul", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_beqr_ul (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_beqi_ul (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("beq.p", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_beqr_p (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_beqi_p (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("beq.ui", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_beqr_ui (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_beqi_ui (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("beq.ul", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_beqr_ul (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_beqi_ul (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bmc.ui", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bmcr_ui (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bmci_ui (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bmc.ul", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bmcr_ul (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bmci_ul (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bmc.ui", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bmcr_ui (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bmci_ui (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bmc.ul", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bmcr_ul (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bmci_ul (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bms.ui", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bmsr_ui (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bmsi_ui (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bms.ul", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bmsr_ul (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bmsi_ul (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bms.ui", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bmsr_ui (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bmsi_ui (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bms.ul", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bmsr_ul (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bmsi_ul (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bge.p", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bger_p (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bgei_p (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bge.p", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bger_p (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bgei_p (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bgt.p", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bgtr_p (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bgti_p (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bgt.p", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bgtr_p (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bgti_p (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("ble.p", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bler_p (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_blei_p (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("ble.p", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bler_p (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_blei_p (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("blt.p", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bltr_p (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_blti_p (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("blt.p", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bltr_p (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_blti_p (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bne.p", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bner_p (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bnei_p (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bne.ui", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bner_ui (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bnei_ui (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bne.ul", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bner_ul (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bnei_ul (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bne.p", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bner_p (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bnei_p (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bne.ui", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bner_ui (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bnei_ui (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bne.ul", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bner_ul (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bnei_ul (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("eq.p", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_eqr_p (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_eqi_p (d, s1, s2);
  }
} else if (!strcmp ("eq.ui", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_eqr_ui (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_eqi_ui (d, s1, s2);
  }
} else if (!strcmp ("eq.ul", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_eqr_ul (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_eqi_ul (d, s1, s2);
  }
} else if (!strcmp ("ge.p", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ger_p (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_gei_p (d, s1, s2);
  }
} else if (!strcmp ("gt.p", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_gtr_p (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_gti_p (d, s1, s2);
  }
} else if (!strcmp ("ld.p", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  d = AS_REG (insn_1);
  if (IS_REG (insn_2)) {
    s1 = AS_REG (insn_2);
    jit_ldr_p (d, s1);
  } else {
    s1 = AS_INT (insn_2);
    jit_ldi_p (d, s1);
  }
} else if (!strcmp ("ldx.p", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ldxr_p (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_ldxi_p (d, s1, s2);
  }
} else if (!strcmp ("le.p", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ler_p (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_lei_p (d, s1, s2);
  }
} else if (!strcmp ("lsh.ui", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_lshr_ui (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_lshi_ui (d, s1, s2);
  }
} else if (!strcmp ("lsh.ul", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_lshr_ul (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_lshi_ul (d, s1, s2);
  }
} else if (!strcmp ("lt.p", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ltr_p (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_lti_p (d, s1, s2);
  }
} else if (!strcmp ("mov.p", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  d = AS_REG (insn_1);
  if (IS_REG (insn_2)) {
    s1 = AS_REG (insn_2);
    jit_movr_p (d, s1);
  } else {
    s1 = AS_INT (insn_2);
    jit_movi_p (d, s1);
  }
} else if (!strcmp ("mov.ui", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  d = AS_REG (insn_1);
  if (IS_REG (insn_2)) {
    s1 = AS_REG (insn_2);
    jit_movr_ui (d, s1);
  } else {
    s1 = AS_INT (insn_2);
    jit_movi_ui (d, s1);
  }
} else if (!strcmp ("mov.ul", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  d = AS_REG (insn_1);
  if (IS_REG (insn_2)) {
    s1 = AS_REG (insn_2);
    jit_movr_ul (d, s1);
  } else {
    s1 = AS_INT (insn_2);
    jit_movi_ul (d, s1);
  }
} else if (!strcmp ("ne.p", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ner_p (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_nei_p (d, s1, s2);
  }
} else if (!strcmp ("ne.ui", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ner_ui (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_nei_ui (d, s1, s2);
  }
} else if (!strcmp ("ne.ul", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ner_ul (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_nei_ul (d, s1, s2);
  }
} else if (!strcmp ("hton.ui", insn_op)) {
  int r1, r2;
  ASSERT_LEN (2);
  r1 = AS_REG (insn_1);
  r2 = AS_REG (insn_2);
  jit_hton_ui (r1, r2);
} else if (!strcmp ("hton.us", insn_op)) {
  int r1, r2;
  ASSERT_LEN (2);
  r1 = AS_REG (insn_1);
  r2 = AS_REG (insn_2);
  jit_hton_us (r1, r2);
} else if (!strcmp ("or.ui", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_orr_ui (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_ori_ui (d, s1, s2);
  }
} else if (!strcmp ("or.ul", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_orr_ul (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_ori_ul (d, s1, s2);
  }
} else if (!strcmp ("pop.ui", insn_op)) {
  int r;
  ASSERT_LEN (1);
  r = AS_REG (insn_1);
  jit_pop_ui (r);
} else if (!strcmp ("pop.ul", insn_op)) {
  int r;
  ASSERT_LEN (1);
  r = AS_REG (insn_1);
  jit_pop_ul (r);
} else if (!strcmp ("push.ui", insn_op)) {
  int r;
  ASSERT_LEN (1);
  r = AS_REG (insn_1);
  jit_push_ui (r);
} else if (!strcmp ("push.ul", insn_op)) {
  int r;
  ASSERT_LEN (1);
  r = AS_REG (insn_1);
  jit_push_ul (r);
} else if (!strcmp ("pusharg.c", insn_op)) {
  int r;
  ASSERT_LEN (1);
  r = AS_REG (insn_1);
  jit_pusharg_c (r);
} else if (!strcmp ("pusharg.p", insn_op)) {
  int r;
  ASSERT_LEN (1);
  r = AS_REG (insn_1);
  jit_pusharg_p (r);
} else if (!strcmp ("pusharg.s", insn_op)) {
  int r;
  ASSERT_LEN (1);
  r = AS_REG (insn_1);
  jit_pusharg_s (r);
} else if (!strcmp ("pusharg.uc", insn_op)) {
  int r;
  ASSERT_LEN (1);
  r = AS_REG (insn_1);
  jit_pusharg_uc (r);
} else if (!strcmp ("pusharg.ui", insn_op)) {
  int r;
  ASSERT_LEN (1);
  r = AS_REG (insn_1);
  jit_pusharg_ui (r);
} else if (!strcmp ("pusharg.ul", insn_op)) {
  int r;
  ASSERT_LEN (1);
  r = AS_REG (insn_1);
  jit_pusharg_ul (r);
} else if (!strcmp ("pusharg.us", insn_op)) {
  int r;
  ASSERT_LEN (1);
  r = AS_REG (insn_1);
  jit_pusharg_us (r);
} else if (!strcmp ("retval.c", insn_op)) {
  int r;
  ASSERT_LEN (1);
  r = AS_REG (insn_1);
  jit_retval_c (r);
} else if (!strcmp ("retval.p", insn_op)) {
  int r;
  ASSERT_LEN (1);
  r = AS_REG (insn_1);
  jit_retval_p (r);
} else if (!strcmp ("retval.s", insn_op)) {
  int r;
  ASSERT_LEN (1);
  r = AS_REG (insn_1);
  jit_retval_s (r);
} else if (!strcmp ("retval.uc", insn_op)) {
  int r;
  ASSERT_LEN (1);
  r = AS_REG (insn_1);
  jit_retval_uc (r);
} else if (!strcmp ("retval.ui", insn_op)) {
  int r;
  ASSERT_LEN (1);
  r = AS_REG (insn_1);
  jit_retval_ui (r);
} else if (!strcmp ("retval.ul", insn_op)) {
  int r;
  ASSERT_LEN (1);
  r = AS_REG (insn_1);
  jit_retval_ul (r);
} else if (!strcmp ("retval.us", insn_op)) {
  int r;
  ASSERT_LEN (1);
  r = AS_REG (insn_1);
  jit_retval_us (r);
} else if (!strcmp ("rsb.ui", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_rsbr_ui (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_rsbi_ui (d, s1, s2);
  }
} else if (!strcmp ("rsb.ul", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_rsbr_ul (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_rsbi_ul (d, s1, s2);
  }
} else if (!strcmp ("st.p", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_str_p (d, s1);
  } else {
    d = AS_INT (insn_1);
    jit_sti_p (d, s1);
  }
} else if (!strcmp ("st.uc", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_str_uc (d, s1);
  } else {
    d = AS_INT (insn_1);
    jit_sti_uc (d, s1);
  }
} else if (!strcmp ("st.ui", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_str_ui (d, s1);
  } else {
    d = AS_INT (insn_1);
    jit_sti_ui (d, s1);
  }
} else if (!strcmp ("st.ul", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_str_ul (d, s1);
  } else {
    d = AS_INT (insn_1);
    jit_sti_ul (d, s1);
  }
} else if (!strcmp ("st.us", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_str_us (d, s1);
  } else {
    d = AS_INT (insn_1);
    jit_sti_us (d, s1);
  }
} else if (!strcmp ("st.p", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_str_p (d, s1);
  } else {
    d = AS_INT (insn_1);
    jit_sti_p (d, s1);
  }
} else if (!strcmp ("st.uc", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_str_uc (d, s1);
  } else {
    d = AS_INT (insn_1);
    jit_sti_uc (d, s1);
  }
} else if (!strcmp ("st.ui", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_str_ui (d, s1);
  } else {
    d = AS_INT (insn_1);
    jit_sti_ui (d, s1);
  }
} else if (!strcmp ("st.ul", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_str_ul (d, s1);
  } else {
    d = AS_INT (insn_1);
    jit_sti_ul (d, s1);
  }
} else if (!strcmp ("st.us", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_str_us (d, s1);
  } else {
    d = AS_INT (insn_1);
    jit_sti_us (d, s1);
  }
} else if (!strcmp ("stx.p", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  s1 = AS_REG (insn_2);
  s2 = AS_REG (insn_3);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_stxr_p (d, s1, s2);
  } else {
    d = AS_INT (insn_1);
    jit_stxi_p (d, s1, s2);
  }
} else if (!strcmp ("stx.uc", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  s1 = AS_REG (insn_2);
  s2 = AS_REG (insn_3);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_stxr_uc (d, s1, s2);
  } else {
    d = AS_INT (insn_1);
    jit_stxi_uc (d, s1, s2);
  }
} else if (!strcmp ("stx.ui", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  s1 = AS_REG (insn_2);
  s2 = AS_REG (insn_3);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_stxr_ui (d, s1, s2);
  } else {
    d = AS_INT (insn_1);
    jit_stxi_ui (d, s1, s2);
  }
} else if (!strcmp ("stx.ul", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  s1 = AS_REG (insn_2);
  s2 = AS_REG (insn_3);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_stxr_ul (d, s1, s2);
  } else {
    d = AS_INT (insn_1);
    jit_stxi_ul (d, s1, s2);
  }
} else if (!strcmp ("stx.us", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  s1 = AS_REG (insn_2);
  s2 = AS_REG (insn_3);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_stxr_us (d, s1, s2);
  } else {
    d = AS_INT (insn_1);
    jit_stxi_us (d, s1, s2);
  }
} else if (!strcmp ("stx.p", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  s1 = AS_REG (insn_2);
  s2 = AS_REG (insn_3);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_stxr_p (d, s1, s2);
  } else {
    d = AS_INT (insn_1);
    jit_stxi_p (d, s1, s2);
  }
} else if (!strcmp ("stx.uc", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  s1 = AS_REG (insn_2);
  s2 = AS_REG (insn_3);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_stxr_uc (d, s1, s2);
  } else {
    d = AS_INT (insn_1);
    jit_stxi_uc (d, s1, s2);
  }
} else if (!strcmp ("stx.ui", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  s1 = AS_REG (insn_2);
  s2 = AS_REG (insn_3);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_stxr_ui (d, s1, s2);
  } else {
    d = AS_INT (insn_1);
    jit_stxi_ui (d, s1, s2);
  }
} else if (!strcmp ("stx.ul", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  s1 = AS_REG (insn_2);
  s2 = AS_REG (insn_3);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_stxr_ul (d, s1, s2);
  } else {
    d = AS_INT (insn_1);
    jit_stxi_ul (d, s1, s2);
  }
} else if (!strcmp ("stx.us", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  s1 = AS_REG (insn_2);
  s2 = AS_REG (insn_3);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_stxr_us (d, s1, s2);
  } else {
    d = AS_INT (insn_1);
    jit_stxi_us (d, s1, s2);
  }
} else if (!strcmp ("sub.p", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_subr_p (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_subi_p (d, s1, s2);
  }
} else if (!strcmp ("sub.ui", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_subr_ui (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_subi_ui (d, s1, s2);
  }
} else if (!strcmp ("sub.ul", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_subr_ul (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_subi_ul (d, s1, s2);
  }
} else if (!strcmp ("xor.ui", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_xorr_ui (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_xori_ui (d, s1, s2);
  }
} else if (!strcmp ("xor.ul", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_xorr_ul (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_xori_ul (d, s1, s2);
  }
} else if (!strcmp ("not.c", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  d = AS_REG (insn_1);
  if (IS_REG (insn_2)) {
    s1 = AS_REG (insn_2);
    jit_notr_c (d, s1);
  } else {
    s1 = AS_INT (insn_2);
    jit_noti_c (d, s1);
  }
} else if (!strcmp ("not.i", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  d = AS_REG (insn_1);
  if (IS_REG (insn_2)) {
    s1 = AS_REG (insn_2);
    jit_notr_i (d, s1);
  } else {
    s1 = AS_INT (insn_2);
    jit_noti_i (d, s1);
  }
} else if (!strcmp ("not.l", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  d = AS_REG (insn_1);
  if (IS_REG (insn_2)) {
    s1 = AS_REG (insn_2);
    jit_notr_l (d, s1);
  } else {
    s1 = AS_INT (insn_2);
    jit_noti_l (d, s1);
  }
} else if (!strcmp ("not", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  d = AS_REG (insn_1);
  if (IS_REG (insn_2)) {
    s1 = AS_REG (insn_2);
    jit_notr_l (d, s1);
  } else {
    s1 = AS_INT (insn_2);
    jit_noti_l (d, s1);
  }
} else if (!strcmp ("not.s", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  d = AS_REG (insn_1);
  if (IS_REG (insn_2)) {
    s1 = AS_REG (insn_2);
    jit_notr_s (d, s1);
  } else {
    s1 = AS_INT (insn_2);
    jit_noti_s (d, s1);
  }
} else if (!strcmp ("not.uc", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  d = AS_REG (insn_1);
  if (IS_REG (insn_2)) {
    s1 = AS_REG (insn_2);
    jit_notr_uc (d, s1);
  } else {
    s1 = AS_INT (insn_2);
    jit_noti_uc (d, s1);
  }
} else if (!strcmp ("not.ui", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  d = AS_REG (insn_1);
  if (IS_REG (insn_2)) {
    s1 = AS_REG (insn_2);
    jit_notr_ui (d, s1);
  } else {
    s1 = AS_INT (insn_2);
    jit_noti_ui (d, s1);
  }
} else if (!strcmp ("not.ul", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  d = AS_REG (insn_1);
  if (IS_REG (insn_2)) {
    s1 = AS_REG (insn_2);
    jit_notr_ul (d, s1);
  } else {
    s1 = AS_INT (insn_2);
    jit_noti_ul (d, s1);
  }
} else if (!strcmp ("not.us", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  d = AS_REG (insn_1);
  if (IS_REG (insn_2)) {
    s1 = AS_REG (insn_2);
    jit_notr_us (d, s1);
  } else {
    s1 = AS_INT (insn_2);
    jit_noti_us (d, s1);
  }
} else if (!strcmp ("rsb.i", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_rsbr_i (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_rsbi_i (d, s1, s2);
  }
} else if (!strcmp ("rsb.l", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_rsbr_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_rsbi_l (d, s1, s2);
  }
} else if (!strcmp ("rsb", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_rsbr_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_rsbi_l (d, s1, s2);
  }
} else if (!strcmp ("add.l", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_addr_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_addi_l (d, s1, s2);
  }
} else if (!strcmp ("add", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_addr_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_addi_l (d, s1, s2);
  }
} else if (!strcmp ("and.l", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_andr_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_andi_l (d, s1, s2);
  }
} else if (!strcmp ("and", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_andr_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_andi_l (d, s1, s2);
  }
} else if (!strcmp ("beq.l", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_beqr_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_beqi_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("beq", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_beqr_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_beqi_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("beq.l", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_beqr_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_beqi_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("beq", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_beqr_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_beqi_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bge.l", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bger_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bgei_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bge", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bger_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bgei_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bge.ul", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bger_ul (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bgei_ul (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bge.l", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bger_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bgei_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bge", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bger_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bgei_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bge.ul", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bger_ul (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bgei_ul (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bgt.l", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bgtr_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bgti_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bgt", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bgtr_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bgti_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bgt.ul", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bgtr_ul (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bgti_ul (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bgt.l", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bgtr_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bgti_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bgt", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bgtr_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bgti_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bgt.ul", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bgtr_ul (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bgti_ul (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("ble.l", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bler_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_blei_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("ble", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bler_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_blei_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("ble.ul", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bler_ul (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_blei_ul (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("ble.l", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bler_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_blei_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("ble", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bler_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_blei_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("ble.ul", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bler_ul (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_blei_ul (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("blt.l", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bltr_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_blti_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("blt", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bltr_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_blti_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("blt.ul", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bltr_ul (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_blti_ul (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("blt.l", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bltr_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_blti_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("blt", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bltr_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_blti_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("blt.ul", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bltr_ul (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_blti_ul (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bosub.l", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bosubr_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bosubi_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bosub", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bosubr_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bosubi_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bosub.ul", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bosubr_ul (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bosubi_ul (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bosub.l", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bosubr_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bosubi_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bosub", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bosubr_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bosubi_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bosub.ul", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bosubr_ul (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bosubi_ul (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("boadd.l", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_boaddr_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_boaddi_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("boadd", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_boaddr_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_boaddi_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("boadd.ul", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_boaddr_ul (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_boaddi_ul (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("boadd.l", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_boaddr_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_boaddi_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("boadd", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_boaddr_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_boaddi_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("boadd.ul", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_boaddr_ul (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_boaddi_ul (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bmc.l", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bmcr_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bmci_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bmc", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bmcr_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bmci_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bmc.l", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bmcr_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bmci_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bmc", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bmcr_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bmci_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bms.l", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bmsr_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bmsi_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bms", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bmsr_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bmsi_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bms.l", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bmsr_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bmsi_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bms", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bmsr_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bmsi_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bne.l", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bner_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bnei_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bne", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bner_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bnei_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bne.l", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bner_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bnei_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("bne", insn_op)) {
  unsigned long s1, s2;
  jit_insn *lab, *ref;
  ASSERT_LEN (3);
  ASSERT_SYM (insn_1);
  lab = get_label_def (label_hash, insn_1);
  if (lab == NULL) lab = jit_forward ();
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    ref = jit_bner_l (lab, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    ref = jit_bnei_l (lab, s1, s2);
  }
  add_label_ref (label_hash, insn_1, ref);
} else if (!strcmp ("div.l", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_divr_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_divi_l (d, s1, s2);
  }
} else if (!strcmp ("div", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_divr_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_divi_l (d, s1, s2);
  }
} else if (!strcmp ("div.ul", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_divr_ul (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_divi_ul (d, s1, s2);
  }
} else if (!strcmp ("eq.l", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_eqr_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_eqi_l (d, s1, s2);
  }
} else if (!strcmp ("eq", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_eqr_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_eqi_l (d, s1, s2);
  }
} else if (!strcmp ("ge.l", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ger_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_gei_l (d, s1, s2);
  }
} else if (!strcmp ("ge", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ger_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_gei_l (d, s1, s2);
  }
} else if (!strcmp ("ge.ul", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ger_ul (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_gei_ul (d, s1, s2);
  }
} else if (!strcmp ("gt.l", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_gtr_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_gti_l (d, s1, s2);
  }
} else if (!strcmp ("gt", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_gtr_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_gti_l (d, s1, s2);
  }
} else if (!strcmp ("gt.ul", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_gtr_ul (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_gti_ul (d, s1, s2);
  }
} else if (!strcmp ("hmul.l", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_hmulr_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_hmuli_l (d, s1, s2);
  }
} else if (!strcmp ("hmul", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_hmulr_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_hmuli_l (d, s1, s2);
  }
} else if (!strcmp ("hmul.ul", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_hmulr_ul (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_hmuli_ul (d, s1, s2);
  }
} else if (!strcmp ("ld.l", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  d = AS_REG (insn_1);
  if (IS_REG (insn_2)) {
    s1 = AS_REG (insn_2);
    jit_ldr_l (d, s1);
  } else {
    s1 = AS_INT (insn_2);
    jit_ldi_l (d, s1);
  }
} else if (!strcmp ("ld", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  d = AS_REG (insn_1);
  if (IS_REG (insn_2)) {
    s1 = AS_REG (insn_2);
    jit_ldr_l (d, s1);
  } else {
    s1 = AS_INT (insn_2);
    jit_ldi_l (d, s1);
  }
} else if (!strcmp ("ld.ui", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  d = AS_REG (insn_1);
  if (IS_REG (insn_2)) {
    s1 = AS_REG (insn_2);
    jit_ldr_ui (d, s1);
  } else {
    s1 = AS_INT (insn_2);
    jit_ldi_ui (d, s1);
  }
} else if (!strcmp ("ldx.l", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ldxr_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_ldxi_l (d, s1, s2);
  }
} else if (!strcmp ("ldx", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ldxr_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_ldxi_l (d, s1, s2);
  }
} else if (!strcmp ("ldx.ui", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ldxr_ui (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_ldxi_ui (d, s1, s2);
  }
} else if (!strcmp ("ldx.ul", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ldxr_ul (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_ldxi_ul (d, s1, s2);
  }
} else if (!strcmp ("le.l", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ler_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_lei_l (d, s1, s2);
  }
} else if (!strcmp ("le", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ler_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_lei_l (d, s1, s2);
  }
} else if (!strcmp ("le.ul", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ler_ul (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_lei_ul (d, s1, s2);
  }
} else if (!strcmp ("lsh.l", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_lshr_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_lshi_l (d, s1, s2);
  }
} else if (!strcmp ("lsh", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_lshr_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_lshi_l (d, s1, s2);
  }
} else if (!strcmp ("lt.l", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ltr_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_lti_l (d, s1, s2);
  }
} else if (!strcmp ("lt", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ltr_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_lti_l (d, s1, s2);
  }
} else if (!strcmp ("lt.ul", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ltr_ul (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_lti_ul (d, s1, s2);
  }
} else if (!strcmp ("mod.l", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_modr_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_modi_l (d, s1, s2);
  }
} else if (!strcmp ("mod", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_modr_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_modi_l (d, s1, s2);
  }
} else if (!strcmp ("mod.ul", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_modr_ul (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_modi_ul (d, s1, s2);
  }
} else if (!strcmp ("mov.l", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  d = AS_REG (insn_1);
  if (IS_REG (insn_2)) {
    s1 = AS_REG (insn_2);
    jit_movr_l (d, s1);
  } else {
    s1 = AS_INT (insn_2);
    jit_movi_l (d, s1);
  }
} else if (!strcmp ("mov", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  d = AS_REG (insn_1);
  if (IS_REG (insn_2)) {
    s1 = AS_REG (insn_2);
    jit_movr_l (d, s1);
  } else {
    s1 = AS_INT (insn_2);
    jit_movi_l (d, s1);
  }
} else if (!strcmp ("mul.l", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_mulr_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_muli_l (d, s1, s2);
  }
} else if (!strcmp ("mul", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_mulr_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_muli_l (d, s1, s2);
  }
} else if (!strcmp ("mul.ul", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_mulr_ul (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_muli_ul (d, s1, s2);
  }
} else if (!strcmp ("ne.l", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ner_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_nei_l (d, s1, s2);
  }
} else if (!strcmp ("ne", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_ner_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_nei_l (d, s1, s2);
  }
} else if (!strcmp ("or.l", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_orr_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_ori_l (d, s1, s2);
  }
} else if (!strcmp ("or", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_orr_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_ori_l (d, s1, s2);
  }
} else if (!strcmp ("pop.l", insn_op)) {
  int r;
  ASSERT_LEN (1);
  r = AS_REG (insn_1);
  jit_pop_l (r);
} else if (!strcmp ("pop", insn_op)) {
  int r;
  ASSERT_LEN (1);
  r = AS_REG (insn_1);
  jit_pop_l (r);
} else if (!strcmp ("push.l", insn_op)) {
  int r;
  ASSERT_LEN (1);
  r = AS_REG (insn_1);
  jit_push_l (r);
} else if (!strcmp ("push", insn_op)) {
  int r;
  ASSERT_LEN (1);
  r = AS_REG (insn_1);
  jit_push_l (r);
} else if (!strcmp ("pusharg.l", insn_op)) {
  int r;
  ASSERT_LEN (1);
  r = AS_REG (insn_1);
  jit_pusharg_l (r);
} else if (!strcmp ("pusharg", insn_op)) {
  int r;
  ASSERT_LEN (1);
  r = AS_REG (insn_1);
  jit_pusharg_l (r);
} else if (!strcmp ("retval.l", insn_op)) {
  int r;
  ASSERT_LEN (1);
  r = AS_REG (insn_1);
  jit_retval_l (r);
} else if (!strcmp ("rsh.l", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_rshr_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_rshi_l (d, s1, s2);
  }
} else if (!strcmp ("rsh", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_rshr_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_rshi_l (d, s1, s2);
  }
} else if (!strcmp ("rsh.ul", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_rshr_ul (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_rshi_ul (d, s1, s2);
  }
} else if (!strcmp ("st.l", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_str_l (d, s1);
  } else {
    d = AS_INT (insn_1);
    jit_sti_l (d, s1);
  }
} else if (!strcmp ("st", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_str_l (d, s1);
  } else {
    d = AS_INT (insn_1);
    jit_sti_l (d, s1);
  }
} else if (!strcmp ("st.l", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_str_l (d, s1);
  } else {
    d = AS_INT (insn_1);
    jit_sti_l (d, s1);
  }
} else if (!strcmp ("st", insn_op)) {
  unsigned long d, s1;
  ASSERT_LEN (2);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_str_l (d, s1);
  } else {
    d = AS_INT (insn_1);
    jit_sti_l (d, s1);
  }
} else if (!strcmp ("stx.l", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  s1 = AS_REG (insn_2);
  s2 = AS_REG (insn_3);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_stxr_l (d, s1, s2);
  } else {
    d = AS_INT (insn_1);
    jit_stxi_l (d, s1, s2);
  }
} else if (!strcmp ("stx", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  s1 = AS_REG (insn_2);
  s2 = AS_REG (insn_3);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_stxr_l (d, s1, s2);
  } else {
    d = AS_INT (insn_1);
    jit_stxi_l (d, s1, s2);
  }
} else if (!strcmp ("stx.l", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  s1 = AS_REG (insn_2);
  s2 = AS_REG (insn_3);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_stxr_l (d, s1, s2);
  } else {
    d = AS_INT (insn_1);
    jit_stxi_l (d, s1, s2);
  }
} else if (!strcmp ("stx", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  s1 = AS_REG (insn_2);
  s2 = AS_REG (insn_3);
  if (IS_REG (insn_1)) {
    d = AS_REG (insn_1);
    jit_stxr_l (d, s1, s2);
  } else {
    d = AS_INT (insn_1);
    jit_stxi_l (d, s1, s2);
  }
} else if (!strcmp ("sub.l", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_subr_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_subi_l (d, s1, s2);
  }
} else if (!strcmp ("sub", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_subr_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_subi_l (d, s1, s2);
  }
} else if (!strcmp ("xor.l", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_xorr_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_xori_l (d, s1, s2);
  }
} else if (!strcmp ("xor", insn_op)) {
  unsigned long d, s1, s2;
  ASSERT_LEN (3);
  d = AS_REG (insn_1);
  s1 = AS_REG (insn_2);
  if (IS_REG (insn_3)) {
    s2 = AS_REG (insn_3);
    jit_xorr_l (d, s1, s2);
  } else {
    s2 = AS_INT (insn_3);
    jit_xori_l (d, s1, s2);
  }
} else {
  scm_misc_error ("assemble", "unrecognized instruction: ~A", SCM_LIST1 (SCM_CAR(insn)));
}

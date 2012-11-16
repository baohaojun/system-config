<?php
/**
 * Internationalisation file for Cite extension.
 *
 * @file
 * @ingroup Extensions
 */

$messages = array();

$messages['en'] = array(
	'cite-desc'                      => 'Adds <nowiki><ref[ name=id]></nowiki> and <nowiki><references/></nowiki> tags, for citations',
	/*
		Debug and errors
	*/
	# Internal errors
	'cite_croak'                     => 'Cite died; $1: $2',
	'cite_error_key_str_invalid'     => 'Internal error;
invalid $str and/or $key.
This should never occur.',
	'cite_error_stack_invalid_input' => 'Internal error;
invalid stack key.
This should never occur.',

	# User errors
	'cite_error'                                     => 'Cite error: $1',
	'cite_error_ref_numeric_key'                     => 'Invalid <code>&lt;ref&gt;</code> tag;
name cannot be a simple integer. Use a descriptive title',
	'cite_error_ref_no_key'                          => 'Invalid <code>&lt;ref&gt;</code> tag;
refs with no content must have a name',
	'cite_error_ref_too_many_keys'                   => 'Invalid <code>&lt;ref&gt;</code> tag;
invalid names, e.g. too many',
	'cite_error_ref_no_input'                        => 'Invalid <code>&lt;ref&gt;</code> tag;
refs with no name must have content',
	'cite_error_references_invalid_parameters'       => 'Invalid <code>&lt;references&gt;</code> tag;
no parameters are allowed.
Use <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Invalid <code>&lt;references&gt;</code> tag;
parameter "group" is allowed only.
Use <code>&lt;references /&gt;</code>, or <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label'        => 'Ran out of custom backlink labels.
Define more in the <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki> message.',
	'cite_error_no_link_label_group'        => 'Ran out of custom link labels for group "$1".
Define more in the <nowiki>[[MediaWiki:$2]]</nowiki> message.',
	'cite_error_references_no_text'                  => 'Invalid <code>&lt;ref&gt;</code> tag;
no text was provided for refs named <code>$1</code>',
	'cite_error_included_ref'                        => 'Closing &lt;/ref&gt; missing for &lt;ref&gt; tag',
	'cite_error_refs_without_references'             => '<code>&lt;ref&gt;</code> tags exist, but no <code>&lt;references/&gt;</code> tag was found',
	'cite_error_group_refs_without_references'       => '<code>&lt;ref&gt;</code> tags exist for a group named "$1", but no corresponding <code>&lt;references group="$1"/&gt;</code> tag was found',
	'cite_error_references_group_mismatch'           => '<code>&lt;ref&gt;</code> tag in <code>&lt;references&gt;</code> has conflicting group attribute "$1".',
	'cite_error_references_missing_group'            => '<code>&lt;ref&gt;</code> tag defined in <code>&lt;references&gt;</code> has group attribute "$1" which does not appear in prior text.',
	'cite_error_references_missing_key'              => '<code>&lt;ref&gt;</code> tag with name "$1" defined in <code>&lt;references&gt;</code> is not used in prior text.',
	'cite_error_references_no_key'                   => '<code>&lt;ref&gt;</code> tag defined in <code>&lt;references&gt;</code> has no name attribute.',
	'cite_error_empty_references_define'             => '<code>&lt;ref&gt;</code> tag defined in <code>&lt;references&gt;</code> with name "$1" has no content.',

	/*
	   Output formatting
	*/
	'cite_reference_link_key_with_num' => '$1_$2',
	# Ids produced by <ref>
	'cite_reference_link_prefix'       => 'cite_ref-',
	'cite_reference_link_suffix'       => '',
	# Ids produced by <references>
	'cite_references_link_prefix'      => 'cite_note-',
	'cite_references_link_suffix'      => '',

	'cite_reference_link'                              => '<sup id="$1" class="reference">[[#$2|<nowiki>[</nowiki>$3<nowiki>]</nowiki>]]</sup>',
	'cite_references_no_link'                          => '<p id="$1">$2</p>',
	'cite_references_link_one'                         => '<li id="$1">[[#$2|↑]] $3</li>',
	'cite_references_link_many'                        => '<li id="$1">↑ $2 $3</li>',
	'cite_references_link_many_format'                 => '<sup>[[#$1|$2]]</sup>',
	# An item from this set is passed as $3 in the message above
	'cite_references_link_many_format_backlink_labels' => 'a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az ba bb bc bd be bf bg bh bi bj bk bl bm bn bo bp bq br bs bt bu bv bw bx by bz ca cb cc cd ce cf cg ch ci cj ck cl cm cn co cp cq cr cs ct cu cv cw cx cy cz da db dc dd de df dg dh di dj dk dl dm dn do dp dq dr ds dt du dv dw dx dy dz ea eb ec ed ee ef eg eh ei ej ek el em en eo ep eq er es et eu ev ew ex ey ez fa fb fc fd fe ff fg fh fi fj fk fl fm fn fo fp fq fr fs ft fu fv fw fx fy fz ga gb gc gd ge gf gg gh gi gj gk gl gm gn go gp gq gr gs gt gu gv gw gx gy gz ha hb hc hd he hf hg hh hi hj hk hl hm hn ho hp hq hr hs ht hu hv hw hx hy hz ia ib ic id ie if ig ih ii ij ik il im in io ip iq ir is it iu iv iw ix iy iz ja jb jc jd je jf jg jh ji jj jk jl jm jn jo jp jq jr js jt ju jv jw jx jy jz ka kb kc kd ke kf kg kh ki kj kk kl km kn ko kp kq kr ks kt ku kv kw kx ky kz la lb lc ld le lf lg lh li lj lk ll lm ln lo lp lq lr ls lt lu lv lw lx ly lz ma mb mc md me mf mg mh mi mj mk ml mm mn mo mp mq mr ms mt mu mv mw mx my mz na nb nc nd ne nf ng nh ni nj nk nl nm nn no np nq nr ns nt nu nv nw nx ny nz oa ob oc od oe of og oh oi oj ok ol om on oo op oq or os ot ou ov ow ox oy oz pa pb pc pd pe pf pg ph pi pj pk pl pm pn po pp pq pr ps pt pu pv pw px py pz qa qb qc qd qe qf qg qh qi qj qk ql qm qn qo qp qq qr qs qt qu qv qw qx qy qz ra rb rc rd re rf rg rh ri rj rk rl rm rn ro rp rq rr rs rt ru rv rw rx ry rz sa sb sc sd se sf sg sh si sj sk sl sm sn so sp sq sr ss st su sv sw sx sy sz ta tb tc td te tf tg th ti tj tk tl tm tn to tp tq tr ts tt tu tv tw tx ty tz ua ub uc ud ue uf ug uh ui uj uk ul um un uo up uq ur us ut uu uv uw ux uy uz va vb vc vd ve vf vg vh vi vj vk vl vm vn vo vp vq vr vs vt vu vv vw vx vy vz wa wb wc wd we wf wg wh wi wj wk wl wm wn wo wp wq wr ws wt wu wv ww wx wy wz xa xb xc xd xe xf xg xh xi xj xk xl xm xn xo xp xq xr xs xt xu xv xw xx xy xz ya yb yc yd ye yf yg yh yi yj yk yl ym yn yo yp yq yr ys yt yu yv yw yx yy yz za zb zc zd ze zf zg zh zi zj zk zl zm zn zo zp zq zr zs zt zu zv zw zx zy zz',
	'cite_references_link_many_sep'                    => "&#32;",
	'cite_references_link_many_and'                    => "&#32;",

	# Although I could just use # instead of <li> above and nothing here that
	# will break on input that contains linebreaks
	'cite_references_prefix' => '<ol class="references">',
	'cite_references_suffix' => '</ol>',
);

/** Message documentation (Message documentation)
 * @author Dani
 * @author Lejonel
 * @author Mormegil
 * @author Nike
 * @author Purodha
 * @author Raimond Spekking
 * @author Raymond
 * @author Siebrand
 * @author The Evil IP address
 */
$messages['qqq'] = array(
	'cite-desc' => '{{desc}}',
	'cite_error_key_str_invalid' => '<tt>$str</tt> and <tt>$key</tt> are literals, and refers to who knows which variables the code uses.',
	'cite_error' => 'Cite extension. This is used when there are errors in ref or references tags. The parameter $1 is an error message.',
	'cite_error_ref_numeric_key' => 'Cite extension. Error message shown if the name of a ref tag only contains digits. Examples that cause this error are <code>&lt;ref name="123" /&gt;</code> or <code>&lt;ref name="456"&gt;input&lt;/ref&gt;</code>',
	'cite_error_ref_no_key' => 'Cite extension. Error message shown when ref tags without any content (that is <code>&lt;ref/&gt;</code>) are used without a name.',
	'cite_error_ref_too_many_keys' => 'Cite extension. Error message shown when ref tags has parameters other than name. Examples that cause this error are <code>&lt;ref name="name" notname="value" /&gt;</code> or <code>&lt;ref notname="value" &gt;input&lt;ref&gt;</code>',
	'cite_error_ref_no_input' => 'Cite extension. Error message shown when ref tags without names have no content. An example that cause this error is <code>&lt;ref&gt;&lt;/ref&gt;</code>',
	'cite_error_references_invalid_parameters' => 'Cite extension. Error message shown when parmeters are used in the references tag. An example that cause this error is <code>&lt;references someparameter="value" /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Cite extension. Error message shown when unknown parameters are used in the references tag. An example that cause this error is <tt><nowiki><references someparameter="value" /></nowiki></tt>',
	'cite_error_references_no_backlink_label' => 'Cite extension. Error message shown in the references tag when the same name is used for too many ref tags. Too many in this case is more than there are backlink labels defined in [[MediaWiki:Cite references link many format backlink labels]].

It is not possible to make a clickable link to this message. "nowiki" is mandatory around [[MediaWiki:Cite references link many format backlink labels]].',
	'cite_error_no_link_label_group' => "*'''$1''' is the name of a reference group.
*'''$2''' is <tt>cite_link_label_group-<i>groupname</i></tt>.",
	'cite_error_references_no_text' => 'Cite extension. This error occurs when the tag <code>&lt;ref name="something" /&gt;</code> is used with the name-option specified and no other tag specifies a cite-text for this name.',
	'cite_error_included_ref' => 'Error message shown if the <tt>&lt;ref&gt;</tt> tag is unbalanced, that means a <tt>&lt;ref&gt;</tt> is not followed by a <tt>&lt;/ref&gt;</tt>',
	'cite_error_references_group_mismatch' => 'Error message shown when doing something like

<pre>
<references group="foo">
<ref group="bar">...</ref>
</references>
</pre>

The <code>$1</code> is the value of the <code>group</code> attribute on the inner <code>&lt;ref&gt;</code> (in the example above, “bar”).',
	'cite_error_references_missing_group' => 'Error message shown when doing something like

<pre>
<references group="foo">
<ref>...</ref>
</references>
</pre>

and there are no <code>&lt;ref&gt;</code> tags in the page text which would use <code>group="foo"</code>.

The <code>$1</code> is the name of the unused <code>group</code> (in the example above, “foo”).',
	'cite_error_references_missing_key' => 'Error message shown when using something like

<pre>
<references>
<ref name="refname">...</ref>
</references>
</pre>

and the reference <code>&lt;ref name="refname" /&gt;</code> is not used anywhere in the page text.

The <code>$1</code> parameter contains the name of the unused reference (in the example above, “refname”).',
	'cite_error_references_no_key' => 'Error message shown when a <code>&lt;ref&gt;</code> inside <code>&lt;references&gt;</code> does not have a <code>name</code> attribute.',
	'cite_error_empty_references_define' => 'Error message shown when there is a <code><ref></code> inside <code><references></code>, but it does not have any content, e.g.

<pre>
<references>
<ref name="foo" />
</references>
</pre>

<code>$1</code> contains the <code>name</code> of the erroneous <code>&lt;ref&gt;</code> (in the above example, “foo”).',
	'cite_reference_link_key_with_num' => '{{optional}}',
	'cite_reference_link_prefix' => '{{optional}}',
	'cite_reference_link_suffix' => '{{optional}}',
	'cite_references_link_prefix' => '{{optional}}',
	'cite_references_link_suffix' => '{{optional}}',
	'cite_reference_link' => '{{optional}}',
	'cite_references_link_one' => '{{optional}}',
	'cite_references_link_many' => '{{optional}}',
	'cite_references_link_many_format' => '{{optional}}',
	'cite_references_link_many_format_backlink_labels' => '{{Optional}}',
	'cite_references_link_many_sep' => '{{optional}}',
	'cite_references_link_many_and' => '{{optional}}',
);

/** Afrikaans (Afrikaans)
 * @author Naudefj
 */
$messages['af'] = array(
	'cite-desc' => 'Maak <nowiki><ref[ name=id]></nowiki> en <nowiki><references/></nowiki> etikette beskikbaar vir sitasie.',
	'cite_croak' => 'Probleem met Cite; $1: $2',
	'cite_error_key_str_invalid' => 'Interne fout;
ongeldige $str en/of $key.
Dit behoort nie te gebeur nie.',
	'cite_error_stack_invalid_input' => 'Interne fout;
ongeldige "stack key".
Dit behoort nie te gebeur nie.',
	'cite_error' => 'Citefout: $1',
	'cite_error_ref_numeric_key' => "Ongeldige etiket <code>&lt;ref&gt;</code>;
die naam kan nie 'n eenvoudige heelgetal wees nie.
Gebruik 'n beskrywende titel",
	'cite_error_ref_no_key' => 'Ongeldige etiket <code>&lt;ref&gt;</code>;
"refs" sonder inhoud moet \'n naam hê',
	'cite_error_ref_too_many_keys' => 'Ongeldig <code>&lt;ref&gt;</code>-etiket;
ongeldige name, byvoorbeeld te veel',
);

/** Aragonese (Aragonés)
 * @author Juanpabl
 */
$messages['an'] = array(
	'cite-desc' => 'Adibe as etiquetas <nowiki><ref[ name=id]></nowiki> y <nowiki><references/></nowiki> ta fer citas',
	'cite_croak' => 'Cita corrompita; $1: $2',
	'cite_error_key_str_invalid' => 'Error interna; $str y/u $key no conforme(s). Isto no habría d\'escaicer nunca.',
	'cite_error_stack_invalid_input' => "Error interna; clau de pila no conforme. Isto no habría d'escaicer nunca.",
	'cite_error' => 'Error en a cita: $1',
	'cite_error_ref_numeric_key' => "Etiqueta <code>&lt;ref&gt;</code> incorreuta; o nombre d'a etiqueta no puede estar un numero entero, faiga servir un títol descriptivo",
	'cite_error_ref_no_key' => 'Etiqueta <code>&lt;ref&gt;</code> incorreuta; as referencias sin de conteniu han de tener un nombre',
	'cite_error_ref_too_many_keys' => 'Etiqueta <code>&lt;ref&gt;</code> incorreuta; nombres de parametros incorreutos.',
	'cite_error_ref_no_input' => 'Etiqueta <code>&lt;ref&gt;</code> incorreuta; as referencias sin nombre no han de tener conteniu',
	'cite_error_references_invalid_parameters' => 'Etiqueta <code>&lt;references&gt;</code> incorreuta; no se premiten parametros, faiga servir <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Etiqueta <code>&lt;references&gt;</code> no conforme;
nomás se premite o parametro "group".
Faiga servir <code>&lt;references /&gt;</code>, u <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Ya no quedan etiquetas backlink presonalizatas, defina más en o mensache <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_references_no_text' => "Etiqueta <code>&lt;ref&gt;</code> incorreuta; no ha escrito garra testo t'as referencias nombratas <code>$1</code>",
	'cite_error_included_ref' => 'Zarrando &lt;/ref&gt; falta una etiqueta &lt;ref&gt;',
);

/** Arabic (العربية)
 * @author Meno25
 * @author OsamaK
 */
$messages['ar'] = array(
	'cite-desc' => 'يضيف وسوم <nowiki><ref[ name=id]></nowiki> و <nowiki><references/></nowiki> ، للاستشهادات',
	'cite_croak' => 'الاستشهاد مات؛ $1: $2',
	'cite_error_key_str_invalid' => 'خطأ داخلي؛
$str و/أو $key غير صحيح.
هذا لا يجب أن يحدث أبدا.',
	'cite_error_stack_invalid_input' => 'خطأ داخلي؛
مفتاح ستاك غير صحيح.
هذا لا يجب أن يحدث أبدا.',
	'cite_error' => 'خطأ استشهاد: $1',
	'cite_error_ref_numeric_key' => 'وسم <code>&lt;ref&gt;</code> غير صحيح؛
الاسم لا يمكن أن يكون عددا صحيحا بسيطا. استخدم عنوانا وصفيا',
	'cite_error_ref_no_key' => 'وسم <code>&lt;ref&gt;</code> غير صحيح؛
المراجع غير ذات المحتوى يجب أن تمتلك اسما',
	'cite_error_ref_too_many_keys' => 'وسم <code>&lt;ref&gt;</code> غير صحيح؛
أسماء غير صحيحة، على سبيل المثال كثيرة جدا',
	'cite_error_ref_no_input' => 'وسم <code>&lt;ref&gt;</code> غير صحيح؛
المراجع غير ذات الاسم يجب أن تمتلك محتوى',
	'cite_error_references_invalid_parameters' => 'وسم <code>&lt;references&gt;</code> غير صحيح؛
لا محددات مسموح بها.
استخدم <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'وسم <code>&lt;references&gt;</code> غير صحيح؛
المحدد "group" فقط مسموح به.
استخدم <code>&lt;references /&gt;</code>، أو <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'نفدت علامات الوصلات الراجعة المخصصة.
عرف المزيد في رسالة <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_references_no_text' => 'وسم <code>&lt;ref&gt;</code> غير صحيح؛
لا نص تم توفيره للمراجع المسماة <code>$1</code>',
	'cite_error_included_ref' => 'إغلاق &lt;/ref&gt; مفقود لوسم &lt;ref&gt;',
	'cite_error_refs_without_references' => 'وسم <code>&lt;ref&gt;</code> موجود، لكن لا وسم <code>&lt;references/&gt;</code> تم العثور عليه',
	'cite_error_group_refs_without_references' => 'وسوم <code>&lt;ref&gt;</code> موجودة لمجموعة اسمها "$1"، لكن لا وسم <code>&lt;references group="$1"/&gt;</code> مماثل تم العثور عليه',
	'cite_error_references_group_mismatch' => 'الوسم <code>&lt;ref&gt;</code> في <code>&lt;references&gt;</code> فيه خاصية group متضاربة "$1".',
	'cite_error_references_missing_group' => 'الوسم <code>&lt;ref&gt;</code> المُعرّف في <code>&lt;references&gt;</code> فيه خاصية group "$1" التي لا تظهر في النص السابق.',
	'cite_error_references_missing_key' => 'الوسم <code>&lt;ref&gt;</code> ذو الاسم "$1" المُعرّف في <code>&lt;references&gt;</code> غير مستخدم في النص السابق.',
	'cite_error_references_no_key' => 'الوسم <code>&lt;ref&gt;</code> المعرف في <code>&lt;references&gt;</code> ليس له خاصة اسم.',
	'cite_error_empty_references_define' => 'الوسم <code>&lt;ref&gt;</code> المُعرّف في <code>&lt;references&gt;</code> بالاسم "$1" ليس له محتوى.',
	'cite_references_link_many_format_backlink_labels' => 'أ ب ت ث ج ح خ د ذ ر ز س ش ص ض ط ظ ع غ ف ق ك ل م ن ه و ي أأ أب أت أث أج أح أخ أد أذ أر أز أس أش أص أض أط أظ أع أغ أف أق أك أل أم أن أه أو أي بأ بب بت بث بج بح بخ بد بذ بر بز بس بش بص بض بط بظ بع بغ بف بق بك بل بم بن به بو بي تأ تب تت تث تج تح تخ تد تذ تر تز تس تش تص تض تط تظ تع تغ تف تق تك تل تم تن ته تو تي ثأ ثب ثت ثث ثج ثح ثخ ثد ثذ ثر ثز ثس ثش ثص ثض ثط ثظ ثع ثغ ثف ثق ثك ثل ثم ثن ثه ثو ثي جأ جب جت جث جج جح جخ جد جذ جر جز جس جش جص جض جط جظ جع جغ جف جق جك جل جم جن جه جو جي حأ حب حت حث حج حح حخ حد حذ حر حز حس حش حص حض حط حظ حع حغ حف حق حك حل حم حن حه حو حي خأ خب خت خث خج خح خخ خد خذ خر خز خس خش خص خض خط خظ خع خغ خف خق خك خل خم خن خه خو خي دأ دب دت دث دج دح دخ دد دذ در دز دس دش دص دض دط دظ دع دغ دف دق دك دل دم دن ده دو دي ذأ ذب ذت ذث ذج ذح ذخ ذد ذذ ذر ذز ذس ذش ذص ذض ذط ذظ ذع ذغ ذف ذق ذك ذل ذم ذن ذه ذو ذي رأ رب رت رث رج رح رخ رد رذ رر رز رس رش رص رض رط رظ رع رغ رف رق رك رل رم رن ره رو ري زأ زب زت زث زج زح زخ زد زذ زر زز زس زش زص زض زط زظ زع زغ زف زق زك زل زم زن زه زو زي سأ سب ست سث سج سح سخ سد سذ سر سز سس سش سص سض سط سظ سع سغ سف سق سك سل سم سن سه سو سي شأ شب شت شث شج شح شخ شد شذ شر شز شس شش شص شض شط شظ شع شغ شف شق شك شل شم شن شه شو شي صأ صب صت صث صج صح صخ صد صذ صر صز صس صش صص صض صط صظ صع صغ صف صق صك صل صم صن صه صو صي ضأ ضب ضت ضث ضج ضح ضخ ضد ضذ ضر ضز ضس ضش ضص ضض ضط ضظ ضع ضغ ضف ضق ضك ضل ضم ضن ضه ضو ضي طأ طب طت طث طج طح طخ طد طذ طر طز طس طش طص طض طط طظ طع طغ طف طق طك طل طم طن طه طو طي ظأ ظب ظت ظث ظج ظح ظخ ظد ظذ ظر ظز ظس ظش ظص ظض ظط ظظ ظع ظغ ظف ظق ظك ظل ظم ظن ظه ظو ظي عأ عب عت عث عج عح عخ عد عذ عر عز عس عش عص عض عط عظ عع عغ عف عق عك عل عم عن عه عو عي غأ غب غت غث غج غح غخ غد غذ غر غز غس غش غص غض غط غظ غع غغ غف غق غك غل غم غن غه غو غي فأ فب فت فث فج فح فخ فد فذ فر فز فس فش فص فض فط فظ فع فغ فف فق فك فل فم فن فه فو في قأ قب قت قث قج قح قخ قد قذ قر قز قس قش قص قض قط قظ قع قغ قف قق قك قل قم قن قه قو قي كأ كب كت كث كج كح كخ كد كذ كر كز كس كش كص كض كط كظ كع كغ كف كق كك كل كم كن كه كو كي لأ لب لت لث لج لح لخ لد لذ لر لز لس لش لص لض لط لظ لع لغ لف لق لك لل لم لن له لو لي مأ مب مت مث مج مح مخ مد مذ مر مز مس مش مص مض مط مظ مع مغ مف مق مك مل مم من مه مو مي نأ نب نت نث نج نح نخ ند نذ نر نز نس نش نص نض نط نظ نع نغ نف نق نك نل نم نن نه نو ني هأ هب هت هث هج هح هخ هد هذ هر هز هس هش هص هض هط هظ هع هغ هف هق هك هل هم هن هه هو هي وأ وب وت وث وج وح وخ ود وذ ور وز وس وش وص وض وط وظ وع وغ وف وق وك ول وم ون وه وو وي يأ يب يت يث يج يح يخ يد يذ ير يز يس يش يص يض يط يظ يع يغ يف يق يك يل يم ين يه يو يي',
);

/** Aramaic (ܐܪܡܝܐ)
 * @author Basharh
 */
$messages['arc'] = array(
	'cite_error' => 'ܦܘܕܐ ܒܡܣܗܕܢܘܬܐ: $1',
);

/** Egyptian Spoken Arabic (مصرى)
 * @author Ghaly
 * @author Meno25
 * @author Ramsis II
 */
$messages['arz'] = array(
	'cite-desc' => 'بيضيف التاجز <nowiki><ref[ name=id]></nowiki> و <nowiki><references/></nowiki> ، للاستشهاد',
	'cite_croak' => 'المرجع مات; $1: $2',
	'cite_error_key_str_invalid' => 'غلط داخلى؛
$str و/أو $key غلط.
ده لازم مايحصلش ابدا.',
	'cite_error_stack_invalid_input' => 'غلط داخلى؛
مفتاح ستاك مش صحيح.
ده لازم مايحصلش ابدا',
	'cite_error' => 'المرجع غلط: $1',
	'cite_error_ref_numeric_key' => 'التاج <code>&lt;ref&gt;</code> مش صحيح؛
الاسم ماينفعش يكون عدد صحيح بسيط. استخدم عنوان بيوصف',
	'cite_error_ref_no_key' => 'التاج <code>&lt;ref&gt;</code> مش صحيح؛
المراجع اللى من غير محتوى لازميكون ليها اسم',
	'cite_error_ref_too_many_keys' => 'التاج <code>&lt;ref&gt;</code> مش صحيح؛
أسامى مش صحيحة، يعنى مثلا: كتير قوي',
	'cite_error_ref_no_input' => 'تاج <code>&lt;ref&gt;</code> مش صحيح؛
المراجع اللى من غير اسم لازم يكون ليها محتوى',
	'cite_error_references_invalid_parameters' => 'مش صحيح <code>&lt;references&gt;</code> تاج;
مافيش محددات مسموح بيها.
استخدم <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'مش صحيح <code>&lt;references&gt;</code> تاج;
محدد "group" مسموح بيه بس.
استخدم <code>&lt;references /&gt;</code>, or <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'علامات الوصلات الراجعة المخصصة خلصت.
عرف اكتر فى رسالة <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_references_no_text' => 'مش صحيح <code>&lt;ref&gt;</code> تاج;
مافيش نص متوافر فى المراجع اللى اسمها<code>$1</code>',
	'cite_error_included_ref' => 'إغلاق &lt;/ref&gt; مفقود لوسم &lt;ref&gt;',
	'cite_error_refs_without_references' => '<code>&lt;ref&gt;</code> التاجز موجوده, بس مافيش <code>&lt;references/&gt;</code> تاجز اتلقت',
	'cite_error_group_refs_without_references' => '<code>&lt;ref&gt;</code> فى تاجز موجوده لمجموعه اسمها "$1", بس مافيش مقابلها تاجز <code>&lt;references group="$1"/&gt;</code> اتلقت',
	'cite_references_link_many_format_backlink_labels' => 'أ ب ت ث ج ح خ د ذ ر ز س ش ص ض ط ظ ع غ ف ق ك ل م ن ه و ى أأ أب أت أث أج أح أخ أد أذ أر أز أس أش أص أض أط أظ أع أغ أف أق أك أل أم أن أه أو أى بأ بب بت بث بج بح بخ بد بذ بر بز بس بش بص بض بط بظ بع بغ بف بق بك بل بم بن به بو بى تأ تب تت تث تج تح تخ تد تذ تر تز تس تش تص تض تط تظ تع تغ تف تق تك تل تم تن ته تو تى ثأ ثب ثت ثث ثج ثح ثخ ثد ثذ ثر ثز ثس ثش ثص ثض ثط ثظ ثع ثغ ثف ثق ثك ثل ثم ثن ثه ثو ثى جأ جب جت جث جج جح جخ جد جذ جر جز جس جش جص جض جط جظ جع جغ جف جق جك جل جم جن جه جو جى حأ حب حت حث حج حح حخ حد حذ حر حز حس حش حص حض حط حظ حع حغ حف حق حك حل حم حن حه حو حى خأ خب خت خث خج خح خخ خد خذ خر خز خس خش خص خض خط خظ خع خغ خف خق خك خل خم خن خه خو خى دأ دب دت دث دج دح دخ دد دذ در دز دس دش دص دض دط دظ دع دغ دف دق دك دل دم دن ده دو دى ذأ ذب ذت ذث ذج ذح ذخ ذد ذذ ذر ذز ذس ذش ذص ذض ذط ذظ ذع ذغ ذف ذق ذك ذل ذم ذن ذه ذو ذى رأ رب رت رث رج رح رخ رد رذ رر رز رس رش رص رض رط رظ رع رغ رف رق رك رل رم رن ره رو رى زأ زب زت زث زج زح زخ زد زذ زر زز زس زش زص زض زط زظ زع زغ زف زق زك زل زم زن زه زو زى سأ سب ست سث سج سح سخ سد سذ سر سز سس سش سص سض سط سظ سع سغ سف سق سك سل سم سن سه سو سى شأ شب شت شث شج شح شخ شد شذ شر شز شس شش شص شض شط شظ شع شغ شف شق شك شل شم شن شه شو شى صأ صب صت صث صج صح صخ صد صذ صر صز صس صش صص صض صط صظ صع صغ صف صق صك صل صم صن صه صو صى ضأ ضب ضت ضث ضج ضح ضخ ضد ضذ ضر ضز ضس ضش ضص ضض ضط ضظ ضع ضغ ضف ضق ضك ضل ضم ضن ضه ضو ضى طأ طب طت طث طج طح طخ طد طذ طر طز طس طش طص طض طط طظ طع طغ طف طق طك طل طم طن طه طو طى ظأ ظب ظت ظث ظج ظح ظخ ظد ظذ ظر ظز ظس ظش ظص ظض ظط ظظ ظع ظغ ظف ظق ظك ظل ظم ظن ظه ظو ظى عأ عب عت عث عج عح عخ عد عذ عر عز عس عش عص عض عط عظ عع عغ عف عق عك عل عم عن عه عو عى غأ غب غت غث غج غح غخ غد غذ غر غز غس غش غص غض غط غظ غع غغ غف غق غك غل غم غن غه غو غى فأ فب فت فث فج فح فخ فد فذ فر فز فس فش فص فض فط فظ فع فغ فف فق فك فل فم فن فه فو فى قأ قب قت قث قج قح قخ قد قذ قر قز قس قش قص قض قط قظ قع قغ قف قق قك قل قم قن قه قو قى كأ كب كت كث كج كح كخ كد كذ كر كز كس كش كص كض كط كظ كع كغ كف كق كك كل كم كن كه كو كى لأ لب لت لث لج لح لخ لد لذ لر لز لس لش لص لض لط لظ لع لغ لف لق لك لل لم لن له لو لى مأ مب مت مث مج مح مخ مد مذ مر مز مس مش مص مض مط مظ مع مغ مف مق مك مل مم من مه مو مى نأ نب نت نث نج نح نخ ند نذ نر نز نس نش نص نض نط نظ نع نغ نف نق نك نل نم نن نه نو نى هأ هب هت هث هج هح هخ هد هذ هر هز هس هش هص هض هط هظ هع هغ هف هق هك هل هم هن هه هو هى وأ وب وت وث وج وح وخ ود وذ ور وز وس وش وص وض وط وظ وع وغ وف وق وك ول وم ون وه وو وى يأ يب يت يث يج يح يخ يد يذ ير يز يس يش يص يض يط يظ يع يغ يف يق يك يل يم ين يه يو يى',
);

/** Asturian (Asturianu)
 * @author Esbardu
 */
$messages['ast'] = array(
	'cite-desc' => 'Añade les etiquetes <nowiki><ref[ name=id]></nowiki> y <nowiki><references/></nowiki> pa les cites',
	'cite_croak' => 'Cita corrompida; $1: $2',
	'cite_error_key_str_invalid' => 'Error internu; $str y/o $key non válidos.  Esto nun habría asoceder nunca.',
	'cite_error_stack_invalid_input' => 'Error internu; clave de pila non válida. Esto nun habría asoceder nunca.',
	'cite_error' => 'Error de cita: $1',
	'cite_error_ref_numeric_key' => 'Etiqueta <code>&lt;ref&gt;</code> non válida; el nome nun pue ser un enteru simple, usa un títulu descriptivu',
	'cite_error_ref_no_key' => 'Etiqueta <code>&lt;ref&gt;</code> non válida; les referencies ensin conteníu han tener un nome',
	'cite_error_ref_too_many_keys' => 'Etiqueta <code>&lt;ref&gt;</code> non válida; nomes non válidos (p.ex. demasiaos)',
	'cite_error_ref_no_input' => 'Etiqueta <code>&lt;ref&gt;</code> non válida; les referencies ensin nome han tener conteníu',
	'cite_error_references_invalid_parameters' => 'Etiqueta <code>&lt;references&gt;</code> non válida; nun se permiten parámetros, usa <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Etiqueta <code>&lt;references&gt;</code> non válida;
namái se permite\'l parámetru "group".
Usa <code>&lt;references /&gt;</code>, o bien <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Etiquetes personalizaes agotaes.
Defini más nel mensaxe <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_references_no_text' => 'Etiqueta <code>&lt;ref&gt;</code> non válida; nun se conseñó testu pa les referencies nomaes <code>$1</code>',
	'cite_error_included_ref' => 'Falta &lt;/ref&gt; pa la etiqueta &lt;ref&gt;',
);

/** Southern Balochi (بلوچی مکرانی)
 * @author Mostafadaneshvar
 */
$messages['bcc'] = array(
	'cite-desc' => 'اضفافه کنت<nowiki><ref[ name=id]></nowiki> و <nowiki><references/></nowiki> تگ, په ارجاع دهگ',
	'cite_croak' => 'ذکر منبع چه بن رپت; $1: $2',
	'cite_error_key_str_invalid' => 'حطا درونی ;
نامعتبرین $str و/یا  $key.
شی نباید هچ وهد پیش کیت',
	'cite_error_stack_invalid_input' => 'درونی حطا;
نامعتربین دسته کلیت.
شی نبایدن هچ وهد پیش کیت.',
	'cite_error' => 'حطا ارجاع: $1',
	'cite_error_ref_numeric_key' => 'نامعتبر <code>&lt;ref&gt;</code>تگ;
نام یک سادگین هوری نه نه بیت. یک توضیحی عنوانی استفاده کنیت',
	'cite_error_ref_no_key' => 'نامعتبر<code>&lt;ref&gt;</code>تگ;
مراجع بی محتوا بایدن نامی داشته بنت',
	'cite_error_ref_too_many_keys' => 'نامعتبر<code>&lt;ref&gt;</code>تگ;
نامعتبر نامان, په داب بازین',
	'cite_error_ref_no_input' => 'نامعتبر <code>&lt;ref&gt;</code> تگ;
مراجع بی نام بایدن محتوا داشته بنت',
	'cite_error_references_invalid_parameters' => 'نامعتبر <code>&lt;references&gt;</code>تگ;
هچ پارامتری مجاز نهنت.
استفاده کن چه <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'نامعتبر <code>&lt;references&gt;</code>تگ;
پارامتر "گروه" فقط مجازنت.
استفاده کن چه <code>&lt;references /&gt;</code>, یا <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'هلگ برجسپان لینک عقب رسمی.
گیشتر تعریف کن ته <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki> کوله',
	'cite_error_references_no_text' => 'نامعتبر<code>&lt;ref&gt;</code>تگ;
په نام ارجاع هچ متنی دهگ نه بیته <code>$1</code>',
	'cite_reference_link_prefix' => 'هل_مرج-',
	'cite_references_link_prefix' => 'ذکرـیادداشت-',
	'cite_references_link_many_format_backlink_labels' => 'ا ب پ ت ج چ خ د ر ز س ش غ ف ک ل م ن و ه ی',
	'cite_references_link_many_sep' => 'س',
	'cite_references_link_many_and' => 'و',
);

/** Belarusian (Taraškievica orthography) (Беларуская (тарашкевіца))
 * @author EugeneZelenko
 * @author Jim-by
 * @author Red Winged Duck
 */
$messages['be-tarask'] = array(
	'cite-desc' => 'Дадае тэгі <nowiki><ref[ name=id]></nowiki> і <nowiki><references/></nowiki> для зносак',
	'cite_croak' => 'Няўдалае цытаваньне; $1: $2',
	'cite_error_key_str_invalid' => 'Унутраная памылка;
няслушны $str і/ці $key.
Гэтага ніколі не павінна быць.',
	'cite_error_stack_invalid_input' => 'Унутраная памылка;
няслушны ключ стэку.
Гэтага ніколі не павінна быць.',
	'cite_error' => 'Памылка цытаваньня: $1',
	'cite_error_ref_numeric_key' => 'Няслушны тэг <code>&lt;ref&gt;</code>;
назва ня можа быць проста лікам, ужывайце апісальную назву',
	'cite_error_ref_no_key' => 'Няслушны тэг <code>&lt;ref&gt;</code>;
пустыя тэгі <code>ref</code> мусяць мець назву',
	'cite_error_ref_too_many_keys' => 'Няслушны тэг <code>&lt;ref&gt;</code>;
няслушныя назвы, ці іх было зашмат',
	'cite_error_ref_no_input' => 'Няслушны тэг <code>&lt;ref&gt;</code>;
крыніцы бяз назваў мусяць мець зьмест',
	'cite_error_references_invalid_parameters' => 'Няслушны тэг <code>&lt;references&gt;</code>;
недазволеныя парамэтры.
Карыстайцеся <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Няслушны тэг <code>&lt;references&gt;</code>;
дазволена карыстацца толькі парамэтрам «group».
Карыстайцеся <code>&lt;references /&gt;</code>, ці <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Не хапае сымбаляў для адваротных спасылак.
Неабходна пашырыць сыстэмнае паведамленьне <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_no_link_label_group' => 'Скончыліся нестандартныя меткі спасылак для групы «$1».
Вызначыце болей у паведамленьні <nowiki>[[MediaWiki:$2]]</nowiki>.',
	'cite_error_references_no_text' => 'Няслушны тэг <code>&lt;ref&gt;</code>;
няма тэксту ў назьве зносак <code>$1</code>',
	'cite_error_included_ref' => 'Няма закрываючага тэга  &lt;/ref&gt; пасьля адкрытага тэга &lt;ref&gt;',
	'cite_error_refs_without_references' => 'Тэг <code>&lt;ref&gt;</code> існуе, але ня знойдзена тэга <code>&lt;references/&gt;</code>',
	'cite_error_group_refs_without_references' => 'Тэг <code>&lt;ref&gt;</code> існуе для групы «$1», але адпаведнага тэга <code>&lt;references group="$1"/&gt;</code> ня знойдзена',
	'cite_error_references_group_mismatch' => 'Тэг <code>&lt;ref&gt;</code> у <code>&lt;references&gt;</code> утрымлівае канфліктуючы атрыбут групы «$1».',
	'cite_error_references_missing_group' => 'Тэг <code>&lt;ref&gt;</code> вызначаны ў <code>&lt;references&gt;</code> утрымлівае атрыбут групы «$1», які раней не выкарыстоўваўся ў тэксьце.',
	'cite_error_references_missing_key' => 'Тэг <code>&lt;ref&gt;</code> з назвай «$1» вызначаны ў <code>&lt;references&gt;</code> не выкарыстоўваўся ў папярэднім тэксьце.',
	'cite_error_references_no_key' => 'Тэг <code>&lt;ref&gt;</code> вызначаны ў <code>&lt;references&gt;</code> ня мае атрыбуту назвы.',
	'cite_error_empty_references_define' => 'Тэг <code>&lt;ref&gt;</code> вызначаны ў <code>&lt;references&gt;</code> з назвай «$1» ня мае зьместу.',
);

/** Bulgarian (Български)
 * @author Borislav
 * @author DCLXVI
 * @author Spiritia
 */
$messages['bg'] = array(
	'cite-desc' => 'Добавя етикетите <nowiki><ref[ name=id]></nowiki> и <nowiki><references/></nowiki>, подходящи за цитиране',
	'cite_croak' => 'Цитиращата система се срути; $1: $2',
	'cite_error_key_str_invalid' => 'Вътрешна грешка: невалиден параметър $str и/или $key.  Това не би трябвало да се случва никога.',
	'cite_error_stack_invalid_input' => "'''Вътрешна грешка:''' невалиден ключ на стека. Това не би трябвало да се случва никога.",
	'cite_error' => 'Грешка при цитиране: $1',
	'cite_error_ref_numeric_key' => "'''Грешка в етикет <code>&lt;ref&gt;</code>:''' името не може да бъде число, използва се описателно име",
	'cite_error_ref_no_key' => "'''Грешка в етикет <code>&lt;ref&gt;</code>:''' етикетите без съдържание трябва да имат име",
	'cite_error_ref_too_many_keys' => "'''Грешка в етикет <code>&lt;ref&gt;</code>:''' грешка в името, например повече от едно име на етикета",
	'cite_error_ref_no_input' => "'''Грешка в етикет <code>&lt;ref&gt;</code>:''' етикетите без име трябва да имат съдържание",
	'cite_error_references_invalid_parameters' => "'''Грешка в етикет <code>&lt;references&gt;</code>:''' използва се без параметри, така: <code>&lt;references /&gt;</code>",
	'cite_error_references_invalid_parameters_group' => 'Невалиден етикет <code>&lt;references&gt;</code>;
позволен е само параметър "group".
Използвайте <code>&lt;references /&gt;</code> или <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Изчерпани са специалните етикети за обратна референция.
Още етикети могат да се дефинират в системното съобщение <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>.',
	'cite_error_references_no_text' => "'''Грешка в етикет <code>&lt;ref&gt;</code>:''' не е подаден текст за бележките на име <code>$1</code>",
	'cite_error_included_ref' => 'Липсва затварящ етикет &lt;/ref&gt; след отварящия етикет &lt;ref&gt;',
	'cite_error_refs_without_references' => 'Присъстват етикети <code>&lt;ref&gt;</code>; липсва етикет <code>&lt;references/&gt;</code>',
	'cite_error_group_refs_without_references' => 'Присъстват етикети <code>&lt;ref&gt;</code> за групата "$1"; но липсва съответният етикет <code>&lt;references group="$1"/&gt;</code>',
);

/** Bengali (বাংলা)
 * @author Bellayet
 * @author Zaheen
 */
$messages['bn'] = array(
	'cite-desc' => 'উদ্ধৃতির জন্য <nowiki><ref[ name=id]></nowiki> এবং <nowiki><references/></nowiki> ট্যাগসমূহ যোগ করুন',
	'cite_croak' => 'উদ্ধৃতি ক্রোক করা হয়েছে; $1: $2',
	'cite_error_key_str_invalid' => 'আভ্যন্তরীন ত্রুটি; অবৈধ $str এবং/অথবা $key। এটা কখনই ঘটা উচিত নয়।',
	'cite_error_stack_invalid_input' => 'আভ্যন্তরীন ত্রুটি; অবৈধ স্ট্যাক কি। এটা কখনই ঘটা উচিত নয়।',
	'cite_error' => 'উদ্ধৃতি ত্রুটি: $1',
	'cite_error_ref_numeric_key' => 'অবৈধ <code>&lt;ref&gt;</code> ট্যাগ; নাম কোন সরল পূর্ণসংখ্যা হতে পারবেনা, একটি বিবরণমূলক শিরোনাম ব্যবহার করুন',
	'cite_error_ref_no_key' => 'অবৈধ <code>&lt;ref&gt;</code> ট্যাগ; বিষয়বস্তুহীন refসমূহের অবশ্যই নাম থাকতে হবে',
	'cite_error_ref_too_many_keys' => 'অবৈধ <code>&lt;ref&gt;</code> ট্যাগ; অবৈধ নাম (যেমন- সংখ্যাতিরিক্ত)',
	'cite_error_ref_no_input' => 'অবৈধ <code>&lt;ref&gt;</code> ট্যাগ; নামবিহীন refসমূহের অবশ্যই বিষয়বস্তু থাকতে হবে',
	'cite_error_references_invalid_parameters' => 'অবৈধ <code>&lt;ref&gt;</code> ট্যাগ; কোন প্যারামিটার অনুমোদিত নয়, <code>&lt;references /&gt;</code> ব্যবহার করুন',
	'cite_error_references_no_backlink_label' => 'পছন্দমাফিক ব্যাকলিংক লেবেলের সংখ্যা ফুরিয়ে গেছে, <nowiki>[[MediaWiki:Cite_references_link_many_format_backlink_labels]]</nowiki> বার্তায় আরও সংজ্ঞায়িত করুন',
	'cite_error_references_no_text' => 'অবৈধ <code>&lt;ref&gt;</code> ট্যাগ; <code>$1</code> নামের refগুলির জন্য কোন টেক্সট প্রদান করা হয়নি',
);

/** Breton (Brezhoneg)
 * @author Fulup
 */
$messages['br'] = array(
	'cite-desc' => 'Ouzhpennañ a ra ar balizennoù <nowiki><ref[ name=id]></nowiki> ha <nowiki><references/></nowiki>, evit an arroudoù.',
	'cite_croak' => 'Arroud breinet ; $1 : $2',
	'cite_error_key_str_invalid' => 'Fazi diabarzh ;
$str ha/pe key$ direizh.
Ne zlefe ket c\'hoarvezout gwezh ebet.',
	'cite_error_stack_invalid_input' => "Fazi diabarzh ;
alc'hwez pil direizh.
Ne zlefe ket c'hoarvezout gwezh ebet.",
	'cite_error' => 'Fazi arroud : $1',
	'cite_error_ref_numeric_key' => "Fazi implijout ar valizenn <code>&lt;ref&gt;</code> ;
n'hall ket an anv bezañ un niver anterin. Grit gant un titl deskrivus",
	'cite_error_ref_no_key' => "Fazi implijout ar valizenn <code>&lt;ref&gt;</code> ;
ret eo d'an daveennoù goullo kaout un anv",
	'cite_error_ref_too_many_keys' => 'Fazi implijout ar valizenn <code>&lt;ref&gt;</code> ;
anv direizh, niver re uhel da skouer',
	'cite_error_ref_no_input' => "Fazi implijout ar valizenn <code>&lt;ref&gt;</code> ;
ret eo d'an daveennoù hep anv bezañ danvez enno",
	'cite_error_references_invalid_parameters' => "Fazi implijout ar valizenn <code>&lt;ref&gt;</code> ;
n'eo aotreet arventenn ebet.
Grit gant ar valizenn <code>&lt;references /&gt;</code>",
	'cite_error_references_invalid_parameters_group' => 'Fazi implijout ar valizenn <code>&lt;ref&gt;</code> ;
n\'eus nemet an arventenn "strollad" zo aotreet.
Grit gant ar valizenn <code>&lt;references /&gt;</code>, pe <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => "N'eus ket a dikedennoù personelaet mui.
Spisait un niver brasoc'h anezho er gemennadenn <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>",
	'cite_error_no_link_label_group' => 'Tikedenn liamm bersonelaet ebet ken evit ar strollad "$1".
Termenit re all e kemennadenn <nowiki>[[MediaWiki:$2]]</nowiki>.',
	'cite_error_references_no_text' => 'Balizenn <code>&lt;ref&gt;</code> direizh ;
ne oa bet lakaet tamm testenn ebet evit ar valizenn <code>$1</code>',
	'cite_error_included_ref' => 'Kod digeriñ &lt;/ref&gt; hep kod serriñ &lt;ref&gt;',
	'cite_error_refs_without_references' => "<code>&lt;ref&gt;</code> balizennoù zo, met n'eus bet kavet balizenn <code>&lt;references/&gt;</code> ebet",
	'cite_error_group_refs_without_references' => '<code>&lt;ref&gt;</code> balizennoù zo evit ur strollad anvet "$1", met n\'eus bet kavet balizenn <code>&lt;references group="$1"/&gt;</code> ebet o klotañ',
	'cite_error_references_group_mismatch' => 'Gant ar valizenn <code>&lt;ref&gt;</code> e <code>&lt;references&gt;</code> emañ an dezverk strollad trubuilhus "$1".',
	'cite_error_references_missing_group' => '<code>&lt;ref&gt;</code> ar valizenn termenet e <code>&lt;references&gt;</code> eo dezhi un dezverk strollad "$1" na gaver ket en destenn a-raok.',
	'cite_error_references_missing_key' => 'N\'eo ket bet implijet en destenn gent ar <code>&lt;ref&gt;</code> valizenn hec\'h anv "$1" termenet e <code>&lt;references&gt;</code>.',
	'cite_error_references_no_key' => "<code>&lt;ref&gt;</code> ar valizenn termenet e <code>&lt;references&gt;</code> n'he deus dezverk anv ebet.",
	'cite_error_empty_references_define' => '<code>&lt;ref&gt;</code> ar valiezenn termenet e <code>&lt;references&gt;</code> dezhi an anv a "$1" zo goullo.',
);

/** Bosnian (Bosanski)
 * @author CERminator
 */
$messages['bs'] = array(
	'cite-desc' => 'Dodaje oznake <nowiki><ref[ name=id]></nowiki> i <nowiki><references/></nowiki> za citiranje',
	'cite_croak' => 'Citiranje neuspješno; $1: $2',
	'cite_error_key_str_invalid' => 'Unutrašnja greška;
nevaljan $str i/ili $key.
Ovo se ne bi trebalo dešavati.',
	'cite_error_stack_invalid_input' => 'Unutrašnja greška;
nepoznat "stack" ključ.
Ovo se ne bi smjelo događati.',
	'cite_error' => 'Greška kod citiranja: $1',
	'cite_error_ref_numeric_key' => 'Nevaljana oznaka <code>&lt;ref&gt;</code>;
naslov ne može biti jednostavni cijeli broj. Koristite opisni naslov',
	'cite_error_ref_no_key' => 'Nevaljana oznaka <code>&lt;ref&gt;</code>;
reference bez sadržaja moraju imati naziv',
	'cite_error_ref_too_many_keys' => 'Nevaljana oznaka <code>&lt;ref&gt;</code>;
nevaljani nazivi, npr. možda ih je previše',
	'cite_error_ref_no_input' => 'Nevaljana oznaka <code>&lt;ref&gt;</code>;
reference bez naziva moraju imati sadržaj',
	'cite_error_references_invalid_parameters' => 'Nevaljana oznaka <code>&lt;references&gt;</code>;
nisu dozvoljeni parametri.
Koristite <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Nevaljana oznaka <code>&lt;references&gt;</code;
dozvoljen je samo parametar "group".
Koristite <code>&lt;references /&gt;</code> ili <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Ponestalo je prilagođenih naslova backlinkova.
Definirajte ih još u <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki> poruci',
	'cite_error_no_link_label_group' => 'Nedovoljan broj proizvoljnih naslova linkova za grupu "$1".
Definišite više putem poruke <nowiki>[[MediaWiki:$2]]</nowiki>.',
	'cite_error_references_no_text' => 'Nevaljana oznaka <code>&lt;ref&gt;</code>;
nije naveden tekst za reference sa imenom <code>$1</code>',
	'cite_error_included_ref' => 'Nedostaje oznaka za zatvaranje &lt;/ref&gt; nakon &lt;ref&gt;',
	'cite_error_refs_without_references' => '<code>&lt;ref&gt;</code> oznake postoje, ali oznaka <code>&lt;references/&gt;</code> nije pronađena',
	'cite_error_group_refs_without_references' => '<code>&lt;ref&gt;</code> oznake postoje za grupu pod imenom "$1", ali nije pronađena pripadajuća oznaka <code>&lt;references group="$1"/&gt;</code>',
	'cite_error_references_group_mismatch' => '<code>&lt;ref&gt;</code> oznaka u <code>&lt;references&gt;</code> ima atribut grupe konflikta "$1".',
	'cite_error_references_missing_group' => '<code>&lt;ref&gt;</code> oznaka definisana u <code>&lt;references&gt;</code> ima atribut grupe "$1" koji se ne pojavljuje u ranijem tekstu.',
	'cite_error_references_missing_key' => '<code>&lt;ref&gt;</code> oznaka sa imenom "$1" definisana u <code>&lt;references&gt;</code> nije korištena u ranijem tekstu.',
	'cite_error_references_no_key' => '<code>&lt;ref&gt;</code> oznaka definisana u <code>&lt;references&gt;</code> nema imenski atribut.',
	'cite_error_empty_references_define' => '<code>&lt;ref&gt;</code> oznaka definisana u <code>&lt;references&gt;</code> sa imenom "$1" nema nikakvog sadržaja.',
);

/** Catalan (Català)
 * @author Davidpar
 * @author Jordi Roqué
 * @author SMP
 * @author Vriullop
 */
$messages['ca'] = array(
	'cite-desc' => 'Afegeix les etiquetes <nowiki><ref[ name=id]></nowiki> i <nowiki><references/></nowiki>, per a cites',
	'cite_croak' => 'Cita corrompuda; $1: $2',
	'cite_error_key_str_invalid' => 'Error intern;
els valors $str i/o $key no valen.
Aquesta situació no s\'hauria de donar mai.',
	'cite_error_stack_invalid_input' => "Error intern;
el valor d'emmagatzematge no és vàlid.
Aquesta situació no s'hauria de donar mai.",
	'cite_error' => 'Error de citació: $1',
	'cite_error_ref_numeric_key' => 'Etiqueta <code>&lt;ref&gt;</code> no vàlida;
el nom no pot ser un nombre. Empreu una paraula o un títol descriptiu',
	'cite_error_ref_no_key' => 'Etiqueta <code>&lt;ref&gt;</code> no vàlida;
les refs sense contingut han de tenir nom',
	'cite_error_ref_too_many_keys' => 'Etiqueta <code>&lt;ref&gt;</code> no vàlida;
empreu l\'estructura <code>&lt;ref name="Nom"&gt;</code>',
	'cite_error_ref_no_input' => 'Etiqueta <code>&lt;ref&gt;</code> no vàlida; 
les referències sense nom han de tenir contingut',
	'cite_error_references_invalid_parameters' => 'Etiqueta <code>&lt;references&gt;</code> no vàlida; 
no es permeten paràmetres. 
Useu <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Etiqueta <code>&lt;references&gt;</code> no vàlida;
únicament es permet el paràmetre "group".
Useu <code>&lt;references /&gt;</code>, o <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => "Hi ha massa etiquetes personalitzades.
Se'n poden definir més a <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>",
	'cite_error_no_link_label_group' => 'No hi ha etiquetes vincle personalitzat per al grup "$1".
Defineix més al missatge <nowiki>[[MediaWiki:$2]]</nowiki>.',
	'cite_error_references_no_text' => "Etiqueta <code>&lt;ref&gt;</code> no vàlida;
no s'ha proporcionat text per les refs amb l'etiqueta <code>$1</code>",
	'cite_error_included_ref' => 'Es tanca el &lt;/ref&gt; que manca per una etiqueta &lt;ref&gt;',
	'cite_error_refs_without_references' => 'Hi ha etiquetes <code>&lt;ref&gt;</code> però no cap etiqueta <code>&lt;references/&gt;</code>',
	'cite_error_group_refs_without_references' => 'Existeixen etiquetes <code>&lt;ref&gt;</code> pel grup «$1» però no l\'etiqueta <code>&lt;references group="$1"/&gt;</code> corresponent',
	'cite_error_references_group_mismatch' => 'L\'etiqueta <code>&lt;ref&gt;</code> a <code>&lt;references&gt;</code> té un conflicte amb l\'atribut de grup "$1".',
	'cite_error_references_missing_group' => 'L\'etiqueta <code>&lt;ref&gt;</code> definida a <code>&lt;references&gt;</code> té l\'atribut de grup "$1" que no apareix en el text anterior.',
	'cite_error_references_missing_key' => 'L\'etiqueta <code>&lt;ref&gt;</code> amb el nom "$1" definida a <code>&lt;references&gt;</code> no s\'utilitza en el text anterior.',
	'cite_error_references_no_key' => "L'etiqueta <code>&lt;ref&gt;</code> definida a <code>&lt;references&gt;</code> no té cap atribut de nom.",
	'cite_error_empty_references_define' => 'L\'etiqueta <code>&lt;ref&gt;</code> definida a <code>&lt;references&gt;</code> amb el nom "$1" no té contingut.',
);

/** Czech (Česky)
 * @author Danny B.
 * @author Li-sung
 * @author Matěj Grabovský
 * @author Mormegil
 * @author Sp5uhe
 */
$messages['cs'] = array(
	'cite-desc' => 'Přidává značky <nowiki><ref[ name="id"]></nowiki> a&nbsp;<nowiki><references /></nowiki> na označení citací',
	'cite_croak' => 'Nefunkční citace; $1: $2',
	'cite_error_key_str_invalid' => 'Vnitřní chyba; neplatný $str nebo $key. Toto by nikdy nemělo nastat.',
	'cite_error_stack_invalid_input' => 'Vnitřní chyba; neplatný klíč zásobníku',
	'cite_error' => 'Chybná citace $1',
	'cite_error_ref_numeric_key' => 'Chyba v tagu <code>&lt;ref&gt;</code>; názvem nesmí být prosté číslo, použijte popisné označení',
	'cite_error_ref_no_key' => 'Chyba v tagu <code>&lt;ref&gt;</code>; prázdné citace musí obsahovat název',
	'cite_error_ref_too_many_keys' => 'Chyba v tagu <code>&lt;ref&gt;</code>; chybné názvy, např. je jich příliš mnoho',
	'cite_error_ref_no_input' => 'Chyba v tagu <code>&lt;ref&gt;</code>; citace bez názvu musí mít vlastní obsah',
	'cite_error_references_invalid_parameters' => 'Chyba v tagu <code>&lt;references&gt;</code>;  zde není dovolen parametr, použijte <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Neplatná značka <tt>&lt;references&gt;</tt>;
je povolen pouze parametr „group“.
Použijte <tt>&lt;references /&gt;</tt> nebo <tt>&lt;references group="..." /&gt;</tt>.',
	'cite_error_references_no_backlink_label' => 'Došla označení zpětných odkazů, přidejte jich několik do zprávy <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_no_link_label_group' => 'Došly definované značky pro skupinu „$1“.
Zvyšte jejich počet ve zprávě <nowiki>[[MediaWiki:$2]]</nowiki>.',
	'cite_error_references_no_text' => 'Chyba v tagu <code>&lt;ref&gt;</code>; citaci označené <code>$1</code> není určen žádný text',
	'cite_error_included_ref' => 'Chybí ukončovací &lt;/ref&gt; k&nbsp;tagu &lt;ref&gt;',
	'cite_error_refs_without_references' => 'Nalezena značka <code>&lt;ref&gt;</code> bez příslušné značky <code>&lt;references/&gt;</code>.',
	'cite_error_group_refs_without_references' => 'Nalezena značka <code>&lt;ref&gt;</code> pro skupinu „$1“ bez příslušné značky <code>&lt;references group="$1"/&gt;</code>.',
	'cite_error_references_group_mismatch' => 'Značka <code>&lt;ref&gt;</code> uvnitř <code>&lt;references&gt;</code> má definovánu jinou skupinu „$1“.',
	'cite_error_references_missing_group' => 'Značka <code>&lt;ref&gt;</code> uvnitř <code>&lt;references&gt;</code> používá skupinu „$1“, která se v předchozím textu neobjevuje.',
	'cite_error_references_missing_key' => 'Na <code>&lt;ref&gt;</code> se jménem „$1“ definovaný uvnitř <code>&lt;references&gt;</code> nejsou v předchozím textu žádné odkazy.',
	'cite_error_references_no_key' => 'U značky <code>&lt;ref&gt;</code> definované uvnitř <code>&lt;references&gt;</code> chybí atribut <code>name</code>.',
	'cite_error_empty_references_define' => 'U značky <code>&lt;ref&gt;</code> s názvem „$1“ definované uvnitř <code>&lt;references&gt;</code> chybí obsah.',
);

/** Church Slavic (Словѣ́ньскъ / ⰔⰎⰑⰂⰡⰐⰠⰔⰍⰟ)
 * @author ОйЛ
 */
$messages['cu'] = array(
	'cite_references_link_many_format_backlink_labels' => 'а б в г д є ж ꙃ ꙁ и і к л м н о п р с т ф х ѡ ц ч ш щ ъ ꙑ ь ѣ ю ꙗ ѥ ѧ ѫ ѩ ѭ ѯ ѱ ѳ ѵ ѷ аа аб ав аг ад ає аж аꙁ аꙃ аи аі ак ал ам ан ао ап ар ас ат аф ах аѡ ац ач аш ащ аъ аꙑ аь аѣ аю аꙗ аѥ аѧ аѫ аѩ аѭ аѯ аѱ аѳ аѵ аѷ',
);

/** Welsh (Cymraeg)
 * @author Lloffiwr
 * @author Xxglennxx
 */
$messages['cy'] = array(
	'cite-desc' => 'Yn ychwanegu tagiau <nowiki><ref[ name=id]></nowiki> a <nowiki><references/></nowiki>, ar gyfer cyfeiriadau',
	'cite_croak' => 'Cyfeirio at farwolaeth; $1: $2',
	'cite_error_key_str_invalid' => 'Gwall mewnol;
$str a/neu $key annilys.
Ni ddylai hyn fyth ddigwydd.',
	'cite_error_stack_invalid_input' => 'Gwall mewnol;
Allwedd pentwr annilys.
Ni ddylai hyn fyth ddigwydd.',
	'cite_error' => 'Gwall cyfeirio: $1',
	'cite_error_ref_numeric_key' => 'Tag <code>&lt;ref&gt;</code> annilys;
ni all enw fod yn rif yn unig. Defnyddiwch deitl disgrifiadol.',
	'cite_error_ref_no_key' => 'Tag <code>&lt;ref&gt;</code> annilys;
rhaid i dagiau ref sydd heb gynnwys iddynt gael enw',
	'cite_error_ref_too_many_keys' => 'Tag <code>&lt;ref&gt;</code> annilys;
enwau annilys; e.e. gormod ohonynt',
	'cite_error_ref_no_input' => 'Tag <code>&lt;ref&gt;</code> annilys;
rhaid i dagiau ref heb enw iddynt gynnwys rhywbeth',
	'cite_error_references_invalid_parameters' => 'Tag <code>&lt;references&gt;</code> annilys;
ni chaniateir paramedrau.
Defnyddiwch <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Tag <code>&lt;references&gt;</code> annilys;
dim ond y paramedr "group" a ganiateir.
Defnyddiwch <code>&lt;references /&gt;</code>, neu <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Dim rhagor o labeli ôl-gyswllt ar gael.
Diffiniwch ragor ohonynt yn y neges <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_no_link_label_group' => 'Wedi rhedeg allan o labeli dolenni unigryw ar gyfer y grŵp "$1".
Gallwch ddiffinio rhagor ohonynt yn y neges <nowiki>[[MediaWiki:$2]]</nowiki>.',
	'cite_error_references_no_text' => "Tag <code>&lt;ref&gt;</code> annilys;
ni osodwyd unrhyw destun ar gyfer y 'ref' <code>$1</code>",
	'cite_error_included_ref' => '&lt;/ref&gt; clo yn eisiau ar gyfer y tag &lt;ref&gt;',
	'cite_error_refs_without_references' => 'Mae tagiau <code>&lt;ref&gt;</code> yn bresennol, ond dim tag <code>&lt;references/&gt;</code>',
	'cite_error_group_refs_without_references' => 'Mae tagiau <code>&lt;ref&gt;</code> yn bresennol ar gyfer y grwp "$1", ond ni chafwyd tag <code>&lt;references/&gt;</code>',
	'cite_error_references_group_mismatch' => 'Mae gan y tag <code>&lt;ref&gt;</code> oddi mewn i <code>&lt;references&gt;</code> briodoledd grŵp anghyson "$1".',
	'cite_error_references_missing_group' => 'Mae gan y tag <code>&lt;ref&gt;</code> a ddiffinir yn <code>&lt;references&gt;</code> briodoledd grŵp "$1" nag ydyw\'n cael ei ddefnyddio yn y testun cynt.',
	'cite_error_references_missing_key' => 'Ni ddefnyddir y tag <code>&lt;ref&gt;</code> o\'r enw "$1", a ddiffinir yn <code>&lt;references&gt;</code>, yn y testun blaenorol.',
	'cite_error_references_no_key' => 'Nid oes dim priodoledd o enw gan y tag <code>&lt;ref&gt;</code> a ddiffinir yn <code>&lt;references&gt;</code>',
	'cite_error_empty_references_define' => 'Does dim byd yn y tag <code>&lt;ref&gt;</code> a\'r enw "$1" arno, sydd wedi ei ddiffinio oddi mewn i dagiau <code>&lt;references&gt;</code>.',
);

/** Danish (Dansk)
 * @author Byrial
 * @author Emilkris33
 * @author Morten LJ
 */
$messages['da'] = array(
	'cite-desc' => 'Tilføjer <nowiki><ref[ name=id]></nowiki> og <nowiki><references/></nowiki>-elementer til referencer.',
	'cite_croak' => 'Fejl i fodnotesystemet; $1: $2',
	'cite_error_key_str_invalid' => 'Intern fejl: Ugyldig $str og/eller $key. Dette burde aldrig forekomme.',
	'cite_error_stack_invalid_input' => 'Intern fejl: Ugyldig staknøgle. Dette burde aldrig forekomme.',
	'cite_error' => 'Fodnotefejl: $1',
	'cite_error_ref_numeric_key' => 'Ugyldigt <code>&lt;ref&gt;</code>-tag; "name" kan ikke være et simpelt heltal, brug en beskrivende titel',
	'cite_error_ref_no_key' => 'Ugyldigt <code>&lt;ref&gt;</code>-tag: Et <code>&lt;ref&gt;</code>-tag uden indhold skal have et navn',
	'cite_error_ref_too_many_keys' => 'Ugyldigt <code>&lt;ref&gt;</code>-tag: Ugyldige navne, fx for mange',
	'cite_error_ref_no_input' => 'Ugyldigt <code>&lt;ref&gt;</code>-tag: Et <code>&lt;ref&gt;</code>-tag uden navn skal have indhold',
	'cite_error_references_invalid_parameters' => 'Ugyldig <code>&lt;references&gt;</code>-tag: Parametre er ikke tilladt, brug i stedet <code>&lt;references /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Løb tør for backlink etiketter. Definer mere i <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki> beskeden',
	'cite_error_references_no_text' => 'Ugyldigt <code>&lt;ref&gt;</code>-tag: Der er ikke specificeret nogen fodnotetekst til navnet <code>$1</code>',
);

/** German (Deutsch)
 * @author Kghbln
 * @author Metalhead64
 * @author Raimond Spekking
 * @author The Evil IP address
 * @author Umherirrender
 */
$messages['de'] = array(
	'cite-desc' => 'Ergänzt die <nowiki><ref[ name=id]></nowiki> und <nowiki><references /></nowiki>-Tags für Quellennachweise',
	'cite_croak' => 'Fehler im Referenz-System. $1: $2',
	'cite_error_key_str_invalid' => 'Interner Fehler: ungültiger $str und/oder $key. Dies sollte eigentlich gar nicht passieren können.',
	'cite_error_stack_invalid_input' => 'Interner Fehler: ungültiger „name“-stack. Dies sollte eigentlich gar nicht passieren können.',
	'cite_error' => 'Referenz-Fehler: $1',
	'cite_error_ref_numeric_key' => 'Ungültige <tt>&lt;ref&gt;</tt>-Verwendung: „name“ darf kein reiner Zahlenwert sein, benutze einen beschreibenden Namen.',
	'cite_error_ref_no_key' => 'Ungültige <tt>&lt;ref&gt;</tt>-Verwendung: „ref“ ohne Inhalt muss einen Namen haben.',
	'cite_error_ref_too_many_keys' => 'Ungültige <tt>&lt;ref&gt;</tt>-Verwendung: „name“ ist ungültig oder zu lang.',
	'cite_error_ref_no_input' => 'Ungültige <tt>&lt;ref&gt;</tt>-Verwendung: „ref“ ohne Namen muss einen Inhalt haben.',
	'cite_error_references_invalid_parameters' => 'Ungültige <tt>&lt;references&gt;</tt>-Verwendung: Es sind keine zusätzlichen Parameter erlaubt, verwende ausschließlich <tt><nowiki><references /></nowiki></tt>.',
	'cite_error_references_invalid_parameters_group' => 'Ungültige <code>&lt;references&gt;</code>-Verwendung: Nur der Parameter „group“ ist erlaubt, verwende <tt>&lt;references /&gt;</tt> oder <tt>&lt;references group="…" /&gt;</tt>',
	'cite_error_references_no_backlink_label' => 'Eine Referenz der Form <tt>&lt;ref name="…" /&gt;</tt> wird öfter benutzt als Buchstaben vorhanden sind. Ein Administrator muss <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki> um weitere Buchstaben/Zeichen ergänzen.',
	'cite_error_no_link_label_group' => 'Es sind für Gruppe „$1“ keine benutzerdefinierte Linkbezeichnungen mehr verfügbar.
Definiere weitere unter Systemtext <nowiki>[[MediaWiki:$2]]</nowiki>.',
	'cite_error_references_no_text' => 'Ungültiger <tt>&lt;ref&gt;</tt>-Tag; es wurde kein Text für das Ref mit dem Namen <tt>$1</tt> angegeben.',
	'cite_error_included_ref' => 'Es fehlt ein schließendes &lt;/ref&gt;',
	'cite_error_refs_without_references' => '<code>&lt;ref&gt;</code>-Tags existieren, jedoch wurde kein <code>&lt;references /&gt;</code>-Tag gefunden.',
	'cite_error_group_refs_without_references' => '<code>&lt;ref&gt;</code>-Tags existieren für die Gruppe „$1“, jedoch wurde kein dazugehöriges <code>&lt;references group="$1" /&gt;</code>-Tag gefunden',
	'cite_error_references_group_mismatch' => 'Das <code>&lt;ref&gt;</code>-Tag in <code>&lt;references&gt;</code> hat das Konfliktgruppenattribut „$1“.',
	'cite_error_references_missing_group' => 'Das in <code>&lt;references&gt;</code> definierte <code>&lt;ref&gt;</code>-Tag hat das Gruppenattribut „$1“, das nicht im vorausgehenden Text vorkommt.',
	'cite_error_references_missing_key' => 'Das in <code>&lt;references&gt;</code> definierte <code>&lt;ref&gt;</code>-Tag mit dem Namen „$1“ wird im vorausgehenden Text nicht verwendet.',
	'cite_error_references_no_key' => 'Das in <code>&lt;references&gt;</code> definierte <code>&lt;ref&gt;</code>-Tag hat kein Namensattribut.',
	'cite_error_empty_references_define' => 'Das in <code>&lt;references&gt;</code> definierte <code>&lt;ref&gt;</code>-Tag mit dem Namen „$1“ weist keinen Inhalt auf.',
);

/** German (formal address) (Deutsch (Sie-Form))
 * @author Imre
 * @author Kghbln
 * @author Raimond Spekking
 */
$messages['de-formal'] = array(
	'cite_error_ref_numeric_key' => 'Ungültige <tt>&lt;ref&gt;</tt>-Verwendung: „name“ darf kein reiner Zahlenwert sein, benutzen Sie einen beschreibenden Namen.',
	'cite_error_references_invalid_parameters' => 'Ungültige <tt>&lt;references&gt;</tt>-Verwendung: Es sind keine zusätzlichen Parameter erlaubt, verwenden Sie ausschließlich <tt><nowiki><references /></nowiki></tt>.',
	'cite_error_references_invalid_parameters_group' => 'Ungültige <code>&lt;references&gt;</code>-Verwendung: Nur der Parameter „group“ ist erlaubt, verwenden Sie <tt>&lt;references /&gt;</tt> oder <tt>&lt;references group="…" /&gt;</tt>',
	'cite_error_no_link_label_group' => 'Es sind für Gruppe „$1“ keine benutzerdefinierte Linkbezeichnungen mehr verfügbar.
Definieren Sie weitere unter Systemtext <nowiki>[[MediaWiki:$2]]</nowiki>.',
);

/** Zazaki (Zazaki)
 * @author Aspar
 * @author Xoser
 */
$messages['diq'] = array(
	'cite-desc' => 'Qe çime mucnayîşî, etiketanê <nowiki><ref[ name=id]></nowiki> u <nowiki><references/></nowiki> de keno',
	'cite_croak' => 'Çime nihebitiyeno; $1: $2',
	'cite_error_key_str_invalid' => 'Ğeletê dehilî
$str raşt niyo u/ya zi $key.
Ena gani nibi.',
	'cite_error_stack_invalid_input' => 'Ğeletê dehilî
Stack key raşt niyo.
Ena gani nibi.',
	'cite_error' => 'Ğeletê çime mucnayîşî: $1',
	'cite_error_ref_numeric_key' => 'Etiket <code>&lt;ref&gt;</code> ke raşt niyo;
Name nieşkeno biyo yew rekam. Çekuyan binuse',
	'cite_error_ref_no_key' => 'Etiket <code>&lt;ref&gt;</code> ke raşt niyo;
Eka kontent çini yo, gani yew name biyo',
	'cite_error_ref_too_many_keys' => 'Etiket <code>&lt;ref&gt;</code> ke raşt niyo;
name raşt niyo, e.g. zaf esto',
	'cite_error_ref_no_input' => 'Etiket <code>&lt;ref&gt;</code> ke raşt niyo;
Eka name çini yo, gani kontent biyo',
	'cite_error_references_invalid_parameters' => 'Etiket <code>&lt;ref&gt;</code> ke raşt niyo;
parametrayan ra destur çini yo.
<code>&lt;references /&gt;</code> sero kar bike',
	'cite_error_references_invalid_parameters_group' => 'Etiket <code>&lt;ref&gt;</code> ke raşt niyo;
parametrayan ra destur çini yo.
<code>&lt;references /&gt;</code> sero kar bike, ya zi  <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Linkanê Custom backlinkî hin çini yo.
Zerreyê mesajê <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>î de hewna tasvir bike',
	'cite_error_no_link_label_group' => 'Eka etiketinê linkê şexsi ser ena grubi "$1" ciniyo.
Zerre mesajê <nowiki>[[MediaWiki:$2]]</nowiki> de zafyer qise bike.',
	'cite_error_references_no_text' => 'Etiket <code>&lt;ref&gt;</code> ke raşt niyo;
qe refs yew nuşte nidayiyo <code>$1</code>',
	'cite_error_included_ref' => '&lt;/ref&gt kefilneno;  &lt;ref&gt vin kerdo; etiket',
	'cite_error_refs_without_references' => 'etiketê <code>&lt;ref&gt;</code>î niesto, feqat  etiketê <code>&lt;references/&gt;</code>î nidiyo',
	'cite_error_group_refs_without_references' => 'etiketê <code>&lt;ref&gt;</code>î niesto ser grupê $1î, feqat  etiketê <code>&lt;references/&gt;</code>î nidiyo',
	'cite_error_references_group_mismatch' => 'etiketê <code>&lt;ref&gt;</code>î, zerre <code>&lt;references/&gt;</code> de ser grupê "$1"î konflikt keno.',
	'cite_error_references_missing_group' => 'etiketê <code>&lt;ref&gt;</code>î, zerre <code>&lt;references/&gt;</code> de tevsir biyo ke ser grupê "$1"î ke verni de nieseno.',
	'cite_error_references_missing_key' => 'etiketê <code>&lt;ref&gt;</code>î, zerre <code>&lt;references/&gt;</code> de tevisr biyo ser name "$1"î verni de niesto.',
	'cite_error_references_no_key' => 'etiketê <code>&lt;ref&gt;</code>î, zerre <code>&lt;references/&gt;</code> de tevsir biyo name xo çini yo.',
	'cite_error_empty_references_define' => 'etiketê <code>&lt;ref&gt;</code>î, zerre <code>&lt;references/&gt;</code> de tevsir biyo "$1" kontent xo çini yo.',
);

/** Lower Sorbian (Dolnoserbski)
 * @author Michawiki
 */
$messages['dsb'] = array(
	'cite-desc' => 'Pśidawa toflicce <nowiki><ref[ name=id]></nowiki> a <nowiki><references/></nowiki> za pódaśa zrědłow',
	'cite_croak' => 'Zmólka w referencnem systemje. $1: $2',
	'cite_error_key_str_invalid' => 'Interna zmólka: njpłaśiwy $str a/abo $key. To njaměło se staś.',
	'cite_error_stack_invalid_input' => 'Interna zmólka: njepłaśiwy stackowy kluc. To njaměło se staś.',
	'cite_error' => 'Referencna zmólka: $1',
	'cite_error_ref_numeric_key' => 'Njepłaśiwa toflicka <code>&lt;ref&gt;</code>;
mě njamóžo jadnora licba byś. Wužyj wugroniwy titel',
	'cite_error_ref_no_key' => 'Njepłaśiwa toflicka <code>&lt;ref&gt;</code>;
"ref" bźez wopśimjeśa musy mě měś',
	'cite_error_ref_too_many_keys' => 'Njepłaśiwa toflicka <code>&lt;ref&gt;</code>;
njepłaśiwe mjenja, na pś. pśewjele',
	'cite_error_ref_no_input' => 'Njepłaśiwa toflicka <code>&lt;ref&gt;</code>;
"ref" bźez mjenja musy wopśimjeśe měś',
	'cite_error_references_invalid_parameters' => 'Njepłaśiwa toflicka <code>&lt;references&gt;</code>;
žedne parametry dowólone.
Wužyj <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Njepłaśiwa toflicka <code>&lt;references&gt;</code>;
jano parameter "group" jo dowólony,
Wužyj <code>&lt;references /&gt;</code> abo <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Swójske etikety slědkwótkazow wupócerane.
Definěruj dalšne w powěsći <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_no_link_label_group' => 'Žedne swójske wótkazowe etikety za "$1" wěcej k dispoziciji.
Definěruj dalšne w powěsći <nowiki>[[MediaWiki:$2]]</nowiki>.',
	'cite_error_references_no_text' => 'Njepłaśiwa toflicka <code>&lt;ref&gt;</code>;
za ref z mjenim <code>$1</code> njejo se tekst pódał',
	'cite_error_included_ref' => 'Kóńceca toflicka &lt;/ref&gt; felujo za toflicku &lt;ref&gt;',
	'cite_error_refs_without_references' => 'Toflicki <code>&lt;ref&gt;</code> eksistěruju, ale toflicka <code>&lt;references/&gt;</code> njejo se namakała',
	'cite_error_group_refs_without_references' => 'Toflicki <code>&lt;ref&gt;</code> eksistěruju za kupku z mjenim "$1", ale wótpowědujuca toflicka <code>&lt;references group="$1"/&gt;</code> njejo se namakała',
	'cite_error_references_group_mismatch' => 'Toflicka <code>&lt;ref&gt;</code> w <code>&lt;references&gt;</code> jo ze kupkowym atributom "$1" w konflikśe.',
	'cite_error_references_missing_group' => 'Toflicka <code>&lt;ref&gt;</code>, kótaraž jo w <code>&lt;references&gt;</code> definěrowana, ma kupkowy atribut "$1", kótaryž njepokazujo se w pjerwjejšnem teksće.',
	'cite_error_references_missing_key' => 'Toflicka <code>&lt;ref&gt;</code> z mjenim "$1", kótaraž jo w <code>&lt;references&gt;</code> definěrowana, njewužywa se w pjerwjejšnem teksće.',
	'cite_error_references_no_key' => 'Toflicka <code>&lt;ref&gt;</code>, kótaraž jo w <code>&lt;references&gt;</code> definěrowana, njama mjenjowy atribut.',
	'cite_error_empty_references_define' => 'Toflicka <code>&lt;ref&gt;</code>, kótaraž jo w <code>&lt;references&gt;</code> z mjenim "$1" definěrowana, njama wopśimjeśe.',
);

/** Greek (Ελληνικά)
 * @author Consta
 * @author Omnipaedista
 * @author ZaDiak
 * @author Απεργός
 */
$messages['el'] = array(
	'cite-desc' => 'Προσθέτει τα <ref[ name="id"]> και <references/> για τις παραπομπές.',
	'cite_croak' => 'Η παραπομπή οδηγεί σε αδιέξοδο; $1: $2',
	'cite_error_key_str_invalid' => 'Εσωτερικό σφάλμα·
μη έγκυρο $str και/ή $key.
Αυτό δεν θα έπρεπε να συμβαίνει.',
	'cite_error_stack_invalid_input' => 'Εσωτερικό σφάλμα·
μη έγκυρο κλειδί στοίβας.
Αυτό δεν θα έπρεπε να συμβαίνει.',
	'cite_error' => 'Σφάλμα αναφοράς: $1',
	'cite_error_ref_numeric_key' => 'Μη έγκυρη <code>&lt;ref&gt;</code> ετικέτα·
το όνομα δεν μπορεί να είναι ένας απλός ακέραιος. Χρησιμοποιήστε έναν περιγραφικό τίτλο',
	'cite_error_ref_no_key' => 'Άκυρη <code>&lt;ref&gt;</code> ετικέτα·
παραπομπές χωρίς περιεχομένο πρέπει να έχουν ένα όνομα',
	'cite_error_ref_too_many_keys' => 'Μη έγκυρη <code>&lt;ref&gt;</code> ετικέτα;
μη έγκυρα ονόματα, π.χ. πάρα πολλά',
	'cite_error_ref_no_input' => 'Μη έγκυρη <code>&lt;ref&gt;</code> ετικέτα;
οι παραπομπές χωρίς όνομα πρέπει να έχουν περιεχόμενο',
	'cite_error_references_invalid_parameters' => 'Μη έγκυρη <code>&lt;references&gt;</code> ετικέτα;
δεν επιτρέπονται παράμετροι.
Χρησιμοποιήστε το <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Μη έγκυρη <code>&lt;references&gt;</code> ετικέτα;
μόνο η παράμετρος "group" επιτρέπεται.
Χρησιμοποιείστε το <code>&lt;references /&gt;</code>, ή το <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Εξαντλήθηκαν οι ειδικές ετικέτες συνδέσμων προς το κείμενο.
Καθορισμός περισσότερων στο μήνυμα <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_no_link_label_group' => 'Εξαντλήθηκαν οι ειδικές ετικέτες συνδέσμων για την ομάδα «$1».
Καθορισμός περισσότερων στο μήνυμα <nowiki>[[MediaWiki:$2]]</nowiki>',
	'cite_error_references_no_text' => 'Άκυρο <code>&lt;ref&gt;</code> tag.
Δεν δίνεται κείμενο για αναφορές με το όνομα <code>$1</code>',
	'cite_error_included_ref' => 'Υπολείπεται η κατάληξη &lt;/ref&gt; για την ετικέτα &lt;ref&gt;',
	'cite_error_refs_without_references' => 'Υπάρχουν ετικέτες <code>&lt;ref&gt;</code>, αλλά καμία ετικέτα <code>&lt;references/&gt;</code> δεν βρέθηκε.',
	'cite_error_group_refs_without_references' => 'Υπάρχουν ετικέτες <code>&lt;ref&gt;</code> για μία ομάδα με το όνομα «$1», αλλά καμία αντίστοιχη ετικέτα <code>&lt;references group="$1"/&gt;</code> δεν βρέθηκε.',
	'cite_error_references_group_mismatch' => 'Η ετικέτα <code>&lt;ref&gt;</code> στο <code>&lt;references&gt;</code> έρχεται σε σύγκρουση με το κατηγορούμενο "$1".',
	'cite_error_references_missing_group' => 'Η <code>&lt;ref&gt;</code> ετικέτα που ορίζεται στο <code>&lt;references&gt;</code> έχει κατηγορούμενο ομάδας "$1" που δεν εμφανίζεται σε προηγούμενο κείμενο.',
	'cite_error_references_missing_key' => 'Η <code>&lt;ref&gt;</code> ετικέτα με το όνομα "$1" που ορίζεται στο <code>&lt;references&gt;</code> δεν χρησιμοποιείται στο προηγούμενο κείμενο.',
	'cite_error_references_no_key' => 'Η <code>&lt;ref&gt;</code> ετικέτα που ορίζεται στο <code>&lt;references&gt;</code> δεν έχει κατηγορούμενο ονόματος.',
	'cite_error_empty_references_define' => 'Η <code>&lt;ref&gt;</code> ετικέτα που ορίζεται στο <code>&lt;references&gt;</code> με το όνομα "$1" δεν έχει καθόλου περιεχόμενο.',
);

/** Esperanto (Esperanto)
 * @author Malafaya
 * @author Yekrats
 */
$messages['eo'] = array(
	'cite-desc' => 'Aldonas etikedojn <nowiki><ref[ name=id]></nowiki> kaj <nowiki><references/></nowiki> por citaĵoj',
	'cite_croak' => 'Cito mortis; $1: $2',
	'cite_error_key_str_invalid' => 'Interna eraro;
malvalida $str kaj/aŭ $key.
Ĉi tio neniam okazos.',
	'cite_error_stack_invalid_input' => 'Interna eraro;
malvalida staka ŝlosilo.
Ĉi tio verŝajne neniam okazus.',
	'cite_error' => 'Citaĵa eraro: $1',
	'cite_error_ref_numeric_key' => 'Malvalida etikedo <code>&lt;ref&gt;</code>;
nomo ne povas esti simpla entjero. Uzu priskriban titolon.',
	'cite_error_ref_no_key' => "Malvalida etikedo <code>&lt;ref&gt;</code>;
''ref'' kun nenia enhava nomo devas havi nomon",
	'cite_error_ref_too_many_keys' => 'Nevalida etikedo <code>&lt;ref&gt;</code>;
nevalidaj nomoj (ekz-e: tro multaj)',
	'cite_error_ref_no_input' => 'Nevalida etikedo <code>&lt;ref&gt;</code>;
ref-etikedoj sen nomo devas havi enhavojn.',
	'cite_error_references_invalid_parameters' => 'Nevalida etikedo <code>&lt;references&gt;</code>; neniuj parametroj estas permesitaj, uzu <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Nevalida etikedon <code>&lt;references&gt;</code>;
parametro "group" nur estas permesita.
Uzu etikedon <code>&lt;references /&gt;</code>, aŭ <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Neniom plu memfaritaj retroligaj etikedoj.
Difinu pliajn en la mesaĝo <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>.',
	'cite_error_no_link_label_group' => 'Mankas proprajn ligilajn etikedojn por grupo "$1".
Difinu pliajn en la <nowiki>[[MediaWiki:$2]]</nowiki> mesaĝo.',
	'cite_error_references_no_text' => 'Nevalida <code>&lt;ref&gt;</code> etikedo;
neniu teksto estis donita por ref-oj nomataj <code>$1</code>',
	'cite_error_included_ref' => 'Ferma &lt;/ref&gt; mankas por &lt;ref&gt; etikedon',
	'cite_error_refs_without_references' => 'Etikedoj <code>&lt;ref&gt;</code> ekzistas, sed neniu etikedo <code>&lt;references/&gt;</code> estis trovita',
	'cite_error_group_refs_without_references' => '<code>&lt;ref&gt;</code> etikedoj ekzistas por grupo nomita "$1", sed ne koresponda <code>&lt;references group="$1"/&gt;</code> etikedo estis trovita',
	'cite_error_references_group_mismatch' => '<code>&lt;ref&gt;</code> etikedo en <code>&lt;references&gt;</code> havas konflikan grupatributon "$1".',
	'cite_error_references_missing_group' => '<code>&lt;ref&gt;</code> etikedo difinita en <code>&lt;references&gt;</code> havas grupatributon "$1" kiu ne aperas en antaŭa teksto.',
	'cite_error_references_missing_key' => '<code>&lt;ref&gt;</code> etikedo kun la nomo "$1" difinita en <code>&lt;references&gt;</code> ne estas uzata en antaŭa teksto.',
	'cite_error_references_no_key' => '<code>&lt;ref&gt;</code> etikedo difinita en <code>&lt;references&gt;</code> ne havas noman atributon.',
	'cite_error_empty_references_define' => '<code>&lt;ref&gt;</code> etikedo difinita en <code>&lt;references&gt;</code> kun nomo "$1" ne havas enhavon.',
);

/** Spanish (Español)
 * @author Antur
 * @author Baiji
 * @author Crazymadlover
 * @author Drini
 * @author Locos epraix
 * @author Manuelt15
 * @author McDutchie
 * @author Muro de Aguas
 * @author Remember the dot
 * @author Sanbec
 * @author Translationista
 */
$messages['es'] = array(
	'cite-desc' => 'Añade las etiquietas <nowiki><ref[ name=id]> y <references /></nowiki> para utilizar notas al pie.',
	'cite_croak' => "La extensión ''Cite'' se murió; $1: $2",
	'cite_error_key_str_invalid' => 'Error interno;
$str y/o $key no válido.
Esto no debería ocurrir.',
	'cite_error_stack_invalid_input' => 'Error interno;
la clave de la pila no es válida.
Esto nunca debe ocurrir.',
	'cite_error' => 'Error en la cita: $1',
	'cite_error_ref_numeric_key' => 'El elemento <code>&lt;ref&gt;</code> no es válido;
el nombre no puede ser un número entero simple. Use un título descriptivo',
	'cite_error_ref_no_key' => 'El elemento <code>&lt;ref&gt;</code> no es válido;
las referencias sin contenido deben tener un nombre',
	'cite_error_ref_too_many_keys' => 'El elemento <code>&lt;ref&gt;</code> no es válido;
nombres de parámetros no válidos',
	'cite_error_ref_no_input' => 'Elemento <code>&lt;ref&gt;</code> no válido;
referencias sin nombre deben tener contenido',
	'cite_error_references_invalid_parameters' => 'El elemento <code>&lt;references&gt;</code> no es válido;
no admite parámetros.
Use <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'El elemento <code>&lt;references&gt;</code> no es válido;
sólo se permite el parámetro «group».
Use <code>&lt;references /&gt;</code>, o <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Se quedó sin etiquetas de vínculos de retroceso personalizadas.
Definir más en el mensaje <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_no_link_label_group' => 'Se han acabado las etiquetas de vínculos personalizados para el grupo "$1".
Define más en el mensaje <nowiki>[[MediaWiki:$2]]</nowiki>.',
	'cite_error_references_no_text' => 'El elemento <code>&lt;ref&gt;</code> no es válido;
pues no hay una referencia con texto llamada <code>$1</code>',
	'cite_error_included_ref' => 'Código de apertura &lt;ref&gt; sin su código de cierre &lt;/ref&gt;',
	'cite_error_refs_without_references' => 'Existen etiquetas <code>&lt;ref&gt;</code>, pero no se encontró una etiqueta <code>&lt;references /&gt;</code>',
	'cite_error_group_refs_without_references' => 'Existen etiquetas <code>&lt;ref&gt;</code> para un grupo llamado "$1", pero no se encontró una etiqueta <code>&lt;references group="$1"/&gt;</code>',
	'cite_error_references_group_mismatch' => 'el tag <code>&lt;ref&gt;</code> en <code>&lt;references&gt;</code> presenta el atributo de grupo en conflicto "$1".',
	'cite_error_references_missing_group' => 'El tag <code>&lt;ref&gt;</code> definido en <code>&lt;references&gt;</code> incluye el atributo "$1" no declarado en el texto precedente.',
	'cite_error_references_missing_key' => 'El tag <code>&lt;ref&gt;</code> con nombre "$1" definido en <code>&lt;references&gt;</code> no se utiliza en el texto precedente.',
	'cite_error_references_no_key' => 'El tag <code>&lt;ref&gt;</code> definido en <code>&lt;references&gt;</code> no tiene atributo de nombre.',
	'cite_error_empty_references_define' => 'El tag <code>&lt;ref&gt;</code> definido en <code>&lt;references&gt;</code> con nombre "$1" no tiene contenido.',
);

/** Estonian (Eesti)
 * @author Pikne
 */
$messages['et'] = array(
	'cite-desc' => 'Lisab viitamiseks märgendid <nowiki><ref[ name=id]></nowiki> ja <nowiki><references/></nowiki>.',
	'cite_error' => 'Viitamistõrge: $1',
	'cite_error_ref_numeric_key' => 'Vigane <code>&lt;ref&gt;</code>-märgend.
Nimi ei või numbriline olla. Kasuta kirjeldavat nime.',
	'cite_error_ref_no_key' => 'Vigane <code>&lt;ref&gt;</code>-märgend.
Sisuta viitamismärgenditel peab nimi olema.',
	'cite_error_ref_too_many_keys' => 'Vigane <code>&lt;ref&gt;</code>-märgend;
"name" on vigane või liiga pikk.',
	'cite_error_ref_no_input' => 'Vigane <code>&lt;ref&gt;</code>-märgend.
Nimeta viitamismärgenditel peab sisu olema.',
	'cite_error_references_invalid_parameters' => 'Vigane <code>&lt;references&gt;</code>-märgend.
Parameetrid pole lubatud.
Kasuta märgendit <code>&lt;references /&gt;</code>.',
	'cite_error_references_invalid_parameters_group' => 'Vigane <code>&lt;references&gt;</code>-märgend.
Lubatud on ainult parameeter "group".
Kasuta märgendit <code>&lt;references /&gt;</code> või <code>&lt;references group="..." /&gt;</code>.',
	'cite_error_references_no_text' => 'Vigane <code>&lt;ref&gt;</code>-märgend.
Viite nimega <code>$1</code> tekst puudub.',
	'cite_error_included_ref' => 'Sulgemismärgend &lt;/ref&gt; puudub.',
	'cite_error_refs_without_references' => '<code>&lt;ref&gt;</code>-märgendid on olemas, aga <code>&lt;references/&gt;</code>-märgend puudub.',
	'cite_error_group_refs_without_references' => 'Olemas on <code>&lt;ref&gt;</code>-märgend rühma "$1" jaoks, aga vastav <code>&lt;references group="$1"/&gt;</code>-märgend puudub.',
	'cite_error_references_group_mismatch' => '<code>&lt;references&gt;</code>-märgendite vahel oleval <code>&lt;ref&gt;</code>-märgendil on vastukäiv parameetri "group" väärtus "$1".',
	'cite_error_references_missing_group' => '<code>&lt;references&gt;</code>-märgendis kirjeldatud <code>&lt;ref&gt;</code>-märgendil on rühmatunnus "$1", mis puudub eelnevas tekstis.',
	'cite_error_references_missing_key' => '<code>&lt;references&gt;</code>-märgendite vahel olevat <code>&lt;ref&gt;</code>-märgendit nimega "$1" ei kasutata eelnevas tekstis.',
	'cite_error_empty_references_define' => '<code>&lt;references&gt;</code>-märgendite vahel oleval <code>&lt;ref&gt;</code>-märgendil nimega "$1" puudub sisu.',
);

/** Basque (Euskara)
 * @author An13sa
 */
$messages['eu'] = array(
	'cite-desc' => '<nowiki><ref[ name=id]></nowiki> eta <nowiki><references/></nowiki> etiketak gehitzen ditu, aipuentzako',
	'cite_croak' => 'Hildako aipua; $1: $2',
	'cite_error' => 'Aipamen errorea: $1',
);

/** Persian (فارسی)
 * @author Ebraminio
 * @author Huji
 * @author Wayiran
 */
$messages['fa'] = array(
	'cite-desc' => 'برچسب‌های <nowiki><ref[ name=id]></nowiki> و <nowiki><references/></nowiki> را برای یادکرد اضافه می‌کند',
	'cite_croak' => 'یادکرد خراب شد؛ $1: $2',
	'cite_error_key_str_invalid' => 'خطای داخلی؛ $str و/یا $key غیر مجاز. این خطا نباید هرگز رخ دهد.',
	'cite_error_stack_invalid_input' => 'خطای داخلی؛ کلید پشته غیرمجاز.  این خطا نباید هرگز رخ دهد.',
	'cite_error' => 'خطای یادکرد: $1',
	'cite_error_ref_numeric_key' => 'برچسب <code><ref></code> غیرمجاز؛ نام نمی‌تواند یک عدد باشد. عنوان واضح‌تری را برگزینید',
	'cite_error_ref_no_key' => 'برچسب <code><ref></code> غیرمجاز؛ یادکردهای بدون محتوا باید نام داشته باشند',
	'cite_error_ref_too_many_keys' => 'برچسب <code><ref></code> غیرمجاز؛ نام‌های غیرمجاز یا بیش از اندازه',
	'cite_error_ref_no_input' => 'برچسب <code><ref></code> غیرمجاز؛ یادکردهای بدون نام باید محتوا داشته باشند',
	'cite_error_references_invalid_parameters' => 'برچسب <code><references></code> غیرمجاز؛ استفاده از پارامتر مجاز است. از <code><references /></code> استفاده کنید',
	'cite_error_references_invalid_parameters_group' => 'برچسب <code>&lt;references&gt;</code> غیر مجاز؛ تنها پارامتر «group» قابل استفاده است.
از <code>&lt;references /&gt;</code> یا <code>&lt;references group="..." /&gt;</code> استفاده کنید',
	'cite_error_references_no_backlink_label' => 'برچسب‌های پیوند به انتها رسید.
موارد جدیدی را در پیام <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki> تعریف کنید',
	'cite_error_no_link_label_group' => 'از برچسب‌های پیوند سفارشی برای گروه «$1» خارج شد.
در پیغام <nowiki>[[MediaWiki:$2]]</nowiki> بیش‌تر تعریف کنید.',
	'cite_error_references_no_text' => 'برچسب <code><ref></code> غیرمجاز؛ متنی برای یادکردهای با نام <code>$1</code> وارد نشده‌است',
	'cite_error_included_ref' => 'برچسب تمام کنندهٔ &lt;/ref&gt; بدون برچسب &lt;ref&gt;',
	'cite_error_refs_without_references' => 'برچسب <code>&lt;ref&gt;</code> وجود دارد اما برچسب <code>&lt;references/&gt;</code> پیدا نشد',
	'cite_error_group_refs_without_references' => 'برچسب <code>&lt;ref&gt;</code> برای گروهی به نام «$1» وجود دارد، اما برچسب <code>&lt;references group="$1"/&gt;</code> متناظر پیدا نشد',
	'cite_error_references_group_mismatch' => 'برچسپ <code>&lt;ref&gt;</code> درون <code>&lt;references&gt;</code> در تضاد با ویژگی‌های گروه «$1» است.',
	'cite_error_references_missing_group' => 'برچسپ <code>&lt;ref&gt;</code> در <code>&lt;references&gt;</code> تعریف شده، ویژگی‌های گروهی «$1» را دارد که درون متن قبل از آن ظاهر نمی‌شود.',
	'cite_error_references_missing_key' => 'پرچسپ <code>&lt;ref&gt;</code> که با نام «$1» درون <code>&lt;references&gt;</code> تعریف شده، در متن قبل از آن استفاده نشده‌است.',
	'cite_error_references_no_key' => 'برچسپ <code>&lt;ref&gt;</code> درون <code>&lt;references&gt;</code> صفت نام را ندارد.',
	'cite_error_empty_references_define' => 'برچسپ <code>&lt;ref&gt;</code> تعریف شده درون <code>&lt;references&gt;</code> با نام «$1» محتوایی ندارد.',
);

/** Finnish (Suomi)
 * @author Agony
 * @author Crt
 * @author Nike
 * @author Olli
 * @author Silvonen
 * @author Str4nd
 * @author Tarmo
 */
$messages['fi'] = array(
	'cite-desc' => 'Tarjoaa <nowiki><ref[ name=id]></nowiki>- ja <nowiki><references/></nowiki>-elementit viittauksien tekemiseen.',
	'cite_croak' => 'Virhe viittausjärjestelmässä: $1: $2',
	'cite_error_key_str_invalid' => 'Sisäinen virhe: kelpaamaton $str ja/tai $key.',
	'cite_error_stack_invalid_input' => 'Sisäinen virhe: kelpaamaton pinoavain.',
	'cite_error' => 'Viittausvirhe: $1',
	'cite_error_ref_numeric_key' => 'Kelpaamaton <code>&lt;ref&gt;</code>-elementti: nimi ei voi olla numero – käytä kuvaavampaa nimeä.',
	'cite_error_ref_no_key' => 'Kelpaamaton <code>&lt;ref&gt;</code>-elementti: sisällöttömille refeille pitää määrittää nimi.',
	'cite_error_ref_too_many_keys' => 'Kelpaamaton <code>&lt;ref&gt;</code>-elementti: virheelliset nimet, esim. liian monta',
	'cite_error_ref_no_input' => 'Kelpaamaton <code>&lt;ref&gt;</code>-elementti: viitteillä ilman nimiä täytyy olla sisältöä',
	'cite_error_references_invalid_parameters' => 'Kelpaamaton <code>&lt;references&gt;</code>-elementti: parametrit eivät ole sallittuja. Käytä muotoa <code>&lt;references /&gt;</code>.',
	'cite_error_references_invalid_parameters_group' => 'Kelpaamaton <code>&lt;references&gt;</code>-elementti: vain parametri ”group” on sallittu. Käytä muotoa <code>&lt;references /&gt;</code> tai <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Määritetyt takaisinviittausnimikkeet loppuivat kesken.
Niitä voi määritellä lisää sivulla <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>.',
	'cite_error_no_link_label_group' => 'Mukautettujen linkkikirjainten määrä ryhmälle ”$1” loppui.
Määritä niitä lisää viestissä <nowiki>[[MediaWiki:$2]]</nowiki>.',
	'cite_error_references_no_text' => 'Virheellinen <code>&lt;ref&gt;</code>-elementti;
viitettä <code>$1</code> ei löytynyt',
	'cite_error_included_ref' => '&lt;ref&gt;-elementin sulkeva &lt;/ref&gt;-elementti puuttuu',
	'cite_error_refs_without_references' => '<code>&lt;ref&gt;</code>-elementti löytyy, mutta <code>&lt;references/&gt;</code>-elementtiä ei löydy',
	'cite_error_group_refs_without_references' => '<code>&lt;ref&gt;</code>-elementit löytyivät ryhmälle nimeltä ”$1”, mutta vastaavaa <code>&lt;references group="$1"/&gt;</code>-elementtiä ei löytynyt',
	'cite_error_references_group_mismatch' => '<code>&lt;ref&gt;</code>-elementti <code>&lt;references&gt;</code>-elementin sisällä sisältää ristiriitaisen ryhmämääritteen ”$1”.',
	'cite_error_references_missing_group' => '<code>&lt;references&gt;</code>-elementissä määritetty <code>&lt;ref&gt;</code>-elementti sisältää ryhmämääritteen ”$1”, jota ei mainita aiemmassa tekstissä.',
	'cite_error_references_missing_key' => '<code>&lt;ref&gt;</code>-elementin nimeä ”$1”, johon viitataan elementissä <code>&lt;references&gt;</code> ei käytetä edeltävässä tekstissä.',
	'cite_error_references_no_key' => '<code>&lt;references&gt;</code>-elementissä määritetyllä <code>&lt;ref&gt;</code>-elementillä ei ole nimimääritettä.',
	'cite_error_empty_references_define' => '<code>&lt;references&gt;</code>-elementissä määritetyllä <code>&lt;ref&gt;</code>-elementillä nimellä ”$1” ei ole sisältöä.',
);

/** French (Français)
 * @author Cedric31
 * @author Crochet.david
 * @author Grondin
 * @author IAlex
 * @author McDutchie
 * @author Sherbrooke
 * @author The Evil IP address
 * @author Verdy p
 */
$messages['fr'] = array(
	'cite-desc' => 'Ajoute les balises <nowiki><ref[ name="id"]></nowiki> et <nowiki><references/></nowiki> pour les citations.',
	'cite_croak' => 'Citation en impasse ; $1 : $2',
	'cite_error_key_str_invalid' => 'Erreur interne ;
$str ou $key invalides.
Ceci ne devrait jamais se produire.',
	'cite_error_stack_invalid_input' => 'Erreur interne ;
clé de pile invalide.
Ceci ne devrait jamais se produire.',
	'cite_error' => 'Erreur de citation : $1',
	'cite_error_ref_numeric_key' => 'Balise <code>&lt;ref&gt;</code> incorrecte ;
le nom ne peut être un entier simple. Utilisez un titre descriptif.',
	'cite_error_ref_no_key' => 'Balise <code>&lt;ref&gt;</code> incorrecte ;
les références sans contenu doivent avoir un nom.',
	'cite_error_ref_too_many_keys' => 'Balise <code>&lt;ref&gt;</code> incorrecte ;
noms incorrects, par exemple trop nombreux.',
	'cite_error_ref_no_input' => 'Balise <code>&lt;ref&gt;</code> incorrecte ;
les références sans nom doivent avoir un contenu.',
	'cite_error_references_invalid_parameters' => "Balise <code>&lt;references&gt;</code> incorrecte ;
aucun paramètre n'est permis.
Utilisez simplement <code>&lt;references /&gt;</code>.",
	'cite_error_references_invalid_parameters_group' => 'Balise <code>&lt;references&gt;</code> incorrecte ;
seul l’attribut « group » est autorisé.
Utilisez <code>&lt;references /&gt;</code>, ou bien <code>&lt;references group="..." /&gt;</code>.',
	'cite_error_references_no_backlink_label' => 'Épuisement des étiquettes de liens personnalisées.
Définissez-en un plus grand nombre dans le message <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>.',
	'cite_error_no_link_label_group' => "Plus d'étiquettes de liens personnalisées pour le groupe « $1 ».
Définissez-en plus dans le message <nowiki>[[MediaWiki:$2]]</nowiki>.",
	'cite_error_references_no_text' => 'Balise <code>&lt;ref&gt;</code> incorrecte ;
aucun texte n’a été fourni pour les références nommées <code>$1</code>.',
	'cite_error_included_ref' => 'Clôture <code>&lt;/ref&gt;</code> manquante pour la balise <code>&lt;ref&gt;</code>.',
	'cite_error_refs_without_references' => 'Des balises <code>&lt;ref&gt;</code> existent, mais aucune balise <code>&lt;references/&gt;</code> n’a été trouvée.',
	'cite_error_group_refs_without_references' => 'Des balises <code>&lt;ref&gt;</code> existent pour un groupe nommé « $1 », mais aucune balise <code>&lt;references group="$1"/&gt;</code> correspondante n’a été trouvée.',
	'cite_error_references_group_mismatch' => "La balise <code>&lt;ref&gt;</code> dans <code>&lt;references&gt;</code> a l'attribut de groupe « $1 » qui entre en conflit avec celui de <code>&lt;references&gt;</code>.",
	'cite_error_references_missing_group' => 'La balise <code>&lt;ref&gt;</code> définie dans <code>&lt;references&gt;</code> a un groupé attribué « $1 » qui ne figure pas dans le texte précédent.',
	'cite_error_references_missing_key' => 'La balise <code>&lt;ref&gt;</code> avec le nom « $1 » définie dans <code>&lt;references&gt;</code> n’est pas utilisé dans le texte précédent.',
	'cite_error_references_no_key' => "La balise <code>&lt;ref&gt;</code> définie dans <code>&lt;references&gt;</code> n’a pas d'attribut de nom.",
	'cite_error_empty_references_define' => 'La balise <code>&lt;ref&gt;</code> défini dans <code>&lt;references&gt;</code> avec le nom « $1 » n’a pas de contenu.',
	'cite_references_link_many_sep' => ',&#32;',
	'cite_references_link_many_and' => '&#32;et&#32;',
);

/** Franco-Provençal (Arpetan)
 * @author ChrisPtDe
 */
$messages['frp'] = array(
	'cite-desc' => 'Apond les balises <nowiki><ref[ name=id]></nowiki> et <nowiki><references/></nowiki>, por les citacions.',
	'cite_croak' => 'Citacion corrompua ; $1 : $2',
	'cite_error_key_str_invalid' => 'Èrror de dedens ; $str atendua.',
	'cite_error_stack_invalid_input' => 'Èrror de dedens ; cllâf de pila envalida.',
	'cite_error' => 'Èrror de citacion $1',
	'cite_error_ref_numeric_key' => 'Apèl envalido ; cllâf pas entègrâla atendua.',
	'cite_error_ref_no_key' => 'Balisa <code>&lt;ref&gt;</code> fôssa ;
les refèrences sen contegnu dêvont avêr un nom.',
	'cite_error_ref_too_many_keys' => 'Apèl envalido ; cllâfs envalides, per ègzemplo, trop de cllâfs spècefiâs ou ben cllâf fôssa.',
	'cite_error_ref_no_input' => 'Balisa <code>&lt;ref&gt;</code> fôssa ;
les refèrences sen nom dêvont avêr un contegnu.',
	'cite_error_references_invalid_parameters' => 'Arguments envalidos ; argument atendu.',
	'cite_error_references_invalid_parameters_group' => 'Balisa <code>&lt;references&gt;</code> fôssa ;
solament lo paramètre « tropa » est ôtorisâ.
Utilisâd <code>&lt;references /&gt;</code>, ou ben <code>&lt;references group="..." /&gt;</code>.',
	'cite_error_references_no_backlink_label' => 'Èpouesement de les ètiquètes de lims pèrsonalisâs.
Dèfenésséd-nen un ples grant nombro dens lo mèssâjo <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>.',
	'cite_error_references_no_text' => 'Balisa <code>&lt;ref&gt;</code> fôssa ;
nion tèxto at étâ balyê por les refèrences apelâs <code>$1</code>.',
	'cite_error_included_ref' => 'Cllotura &lt;/ref&gt; manquenta por la balisa &lt;ref&gt;.',
	'cite_error_refs_without_references' => 'Des balises <code>&lt;ref&gt;</code> ègzistont, mas niona balisa <code>&lt;references/&gt;</code> at étâ trovâ.',
);

/** Friulian (Furlan)
 * @author Klenje
 */
$messages['fur'] = array(
	'cite_error' => 'Erôr te funzion Cite: $1',
);

/** Galician (Galego)
 * @author Alma
 * @author Toliño
 * @author Xosé
 */
$messages['gl'] = array(
	'cite-desc' => 'Engade <nowiki><ref[ nome=id]></nowiki> e etiquetas <nowiki><references/></nowiki>, para notas',
	'cite_croak' => 'Cita morta; $1: $2',
	'cite_error_key_str_invalid' => 'Erro interno; $str e/ou $key inválidos. Isto non debera ocorrer.',
	'cite_error_stack_invalid_input' => 'Erro interno; stack key inválido. Isto non debera ocorrer.',
	'cite_error' => 'Erro no código da cita: $1',
	'cite_error_ref_numeric_key' => 'etiqueta <code>&lt;ref&gt;</code> non válida;
o nome non pode ser un simple entero: use un título descritivo',
	'cite_error_ref_no_key' => 'etiqueta <code>&lt;ref&gt;</code> non válida;
as referencias que non teñan contido deben ter un nome',
	'cite_error_ref_too_many_keys' => 'etiqueta <code>&lt;ref&gt;</code> non válida;
nomes non válidos, por exemplo, demasiados',
	'cite_error_ref_no_input' => 'etiqueta <code>&lt;ref&gt;</code> non válida;
as referencias que non teñan nome, deben ter contido',
	'cite_error_references_invalid_parameters' => 'etiqueta <code>&lt;references&gt;</code> non válida;
non están permitidos eses parámetros.
Use <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'etiqueta <code>&lt;references&gt;</code> non válida;
só está permitido o parámetro "group" ("grupo").
Use <code>&lt;references /&gt;</code> ou <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'As etiquetas personalizadas esgotáronse.
Defina máis na mensaxe <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_no_link_label_group' => 'As etiquetas personalizadas esgotáronse para o grupo "$1".
Defina máis na mensaxe <nowiki>[[MediaWiki:$2]]</nowiki>.',
	'cite_error_references_no_text' => 'etiqueta <code>&lt;ref&gt;</code> non válida;
non se forneceu texto para as referencias de nome <code>$1</code>',
	'cite_error_included_ref' => 'peche a etiqueta &lt;/ref&gt; que lle falta á outra etiqueta &lt;ref&gt;',
	'cite_error_refs_without_references' => 'as etiquetas <code>&lt;ref&gt;</code> existen, pero non se atopou ningunha etiqueta <code>&lt;references/&gt;</code>',
	'cite_error_group_refs_without_references' => 'as etiquetas <code>&lt;ref&gt;</code> existen para un grupo chamado "$1", pero non se atopou a etiqueta <code>&lt;references group="$1"/&gt;</code> correspondente',
	'cite_error_references_group_mismatch' => 'A etiqueta <code>&lt;ref&gt;</code> en <code>&lt;references&gt;</code> ten un atributo de grupo conflitivo "$1".',
	'cite_error_references_missing_group' => 'A etiqueta <code>&lt;ref&gt;</code> definida en <code>&lt;references&gt;</code> ten un atributo de grupo "$1" que non aparece no texto anterior.',
	'cite_error_references_missing_key' => 'A etiqueta <code>&lt;ref&gt;</code> co nome "$1" definida en <code>&lt;references&gt;</code> non se utiliza no texto anterior.',
	'cite_error_references_no_key' => 'A etiqueta <code>&lt;ref&gt;</code> definida en <code>&lt;references&gt;</code> non ten nome de atributo.',
	'cite_error_empty_references_define' => 'A etiqueta <code>&lt;ref&gt;</code> definida en <code>&lt;references&gt;</code> co nome "$1" non ten contido.',
);

/** Ancient Greek (Ἀρχαία ἑλληνικὴ)
 * @author Omnipaedista
 */
$messages['grc'] = array(
	'cite_error' => 'Σφάλμα μνείας: $1',
);

/** Swiss German (Alemannisch)
 * @author Als-Holder
 * @author The Evil IP address
 */
$messages['gsw'] = array(
	'cite-desc' => 'Ergänzt d <nowiki><ref[ name=id]></nowiki> un d <nowiki><references /></nowiki>-Tag fir Quällenochwyys',
	'cite_croak' => 'Fähler im Referenz-Syschtem. $1: $2',
	'cite_error_key_str_invalid' => 'Intärne Fähler: uugiltige $str un/oder $key. Des sott eigetli gar nit chenne gschäh.',
	'cite_error_stack_invalid_input' => 'Intärne Fähler: uugiltige „name“-stack. Des sott eigetli gar nit chenne gschäh.',
	'cite_error' => 'Referänz-Fähler: $1',
	'cite_error_ref_numeric_key' => 'Uugiltigi <tt>&lt;ref&gt;</tt>-Verwändig: „name“ derf kei reine Zahlewärt syy, verwänd e Name wu bschrybt.',
	'cite_error_ref_no_key' => 'Uugiltigi <tt>&lt;ref&gt;</tt>-Verwändig: „ref“ ohni Inhalt muess e Name haa.',
	'cite_error_ref_too_many_keys' => 'Uugiltigi <tt>&lt;ref&gt;</tt>-Verwändig: „name“ isch uugiltig oder z lang.',
	'cite_error_ref_no_input' => 'Uugiltigi <tt>&lt;ref&gt;</tt>-Verwändig: „ref“ ohni Name muess e Inhalt haa.',
	'cite_error_references_invalid_parameters' => 'Uugiltigi <tt>&lt;references&gt;</tt>-Verwändig: S sin kei zuesätzligi Parameter erlaubt, verwänd usschließli <tt><nowiki><references /></nowiki></tt>.',
	'cite_error_references_invalid_parameters_group' => 'Uugiltigi <tt>&lt;references&gt;</tt>-Verwändig: Nume dr Parameter „group“ isch erlaubt, verwänd <tt>&lt;references /&gt;</tt> oder <tt>&lt;references group="..." /&gt;</tt>',
	'cite_error_references_no_backlink_label' => 'E Referenz mit dr Form <tt>&lt;ref name="..."/&gt;</tt> wird meh brucht as es Buechstabe git. E Ammann muess  <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki> go wyteri Buechstabe/Zeiche ergänze.',
	'cite_error_no_link_label_group' => 'Fir d Gruppe „$1“ sin kei benutzerdefinierti Linkbezeichnige me verfiegbar.
Definier meh unter Systemnochricht <nowiki>[[MediaWiki:$2]]</nowiki>.',
	'cite_error_references_no_text' => 'Uugiltige <tt>&lt;ref&gt;</tt>-Tag; s isch kei Täxt fir s Ref mit em Name <tt>$1</tt> aagee wore.',
	'cite_error_included_ref' => 'S fählt s schließend &lt;/ref&gt;',
	'cite_error_refs_without_references' => '<code>&lt;ref&gt;</code>-Tag git s, aber s isch kei <code>&lt;references/&gt;</code>-Tag gfunde wore.',
	'cite_error_group_refs_without_references' => '<code>&lt;ref&gt;</code>-Tag git s fir d Grupp „$1“, aber s isch kei dezue gherig <code>&lt;references group=„$1“/&gt;</code>-Tag gfunde wore',
	'cite_error_references_group_mismatch' => 'Im <code>&lt;ref&gt;</code>-Tag in <code>&lt;references&gt;</code> het s e problematischi Gruppe-Eigeschaft  „$1“.',
	'cite_error_references_missing_group' => 'Im <code>&lt;ref&gt;</code>-Tag, wu definiert isch in <code>&lt;references&gt;</code>, het s e Gruppe-Eigeschaft „$1“, wu im obere Text nit vorchunnt.',
	'cite_error_references_missing_key' => 'S <code>&lt;ref&gt;</code>-Tag mit em Name „$1“, wu definiert isch in <code>&lt;references&gt;</code> wird nit verwändet im obere Text.',
	'cite_error_references_no_key' => 'S <code>&lt;ref&gt;</code>-Tag, wu definiert isch in <code>&lt;references&gt;</code>, het kei Name-Eigeschaft.',
	'cite_error_empty_references_define' => 'Im <code>&lt;ref&gt;</code>-Tag, wu definiert isch in <code>&lt;references&gt;</code>, mit em Name „$1“ het s kei Inhalt.',
);

/** Gujarati (ગુજરાતી)
 * @author Dsvyas
 */
$messages['gu'] = array(
	'cite_references_link_many_format_backlink_labels' => '',
);

/** Hebrew (עברית)
 * @author Amire80
 * @author Rotem Liss
 * @author Rotemliss
 * @author YaronSh
 */
$messages['he'] = array(
	'cite-desc' => 'הוספת תגיות <span dir="ltr"><nowiki><ref[ name=id]></nowiki></span> ו־<nowiki><references/></nowiki> עבור הערות שוליים',
	'cite_croak' => 'בהערה יש שגיאה; $1: $2',
	'cite_error_key_str_invalid' => 'שגיאה פנימית; $str ו/או $key שגויים. זהו באג בתוכנה.',
	'cite_error_stack_invalid_input' => 'שגיאה פנימית; מפתח שגוי במחסנית. זהו באג בתוכנה.',
	'cite_error' => 'שגיאת ציטוט: $1',
	'cite_error_ref_numeric_key' => 'תגית <code>&lt;ref&gt;</code> שגויה; שם לא יכול להיות מספר פשוט, יש להשתמש בכותרת תיאורית',
	'cite_error_ref_no_key' => 'תגית <code>&lt;ref&gt;</code> שגויה; להערות שוליים ללא תוכן חייב להיות שם',
	'cite_error_ref_too_many_keys' => 'תגית <code>&lt;ref&gt;</code> שגויה; שמות שגויים, למשל, רבים מדי',
	'cite_error_ref_no_input' => 'תגית <code>&lt;ref&gt;</code> שגויה; להערות שוליים ללא שם חייב להיות תוכן',
	'cite_error_references_invalid_parameters' => 'תגית <code>&lt;references&gt;</code> שגויה; לא ניתן להשתמש בפרמטרים, יש להשתמש בקוד <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'תגית <code>&lt;references&gt;</code> שגויה;
רק הפרמטר "group" מותר לשימוש.
אנא השתמשו בקוד <code>&lt;references /&gt;</code>, או בקוד <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'נגמרו תוויות הקישורים המותאמים אישית, אנא הגדירו נוספים בהודעת המערכת <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_no_link_label_group' => 'תוויות הקישורים המותאמים אישית לקבוצה "$1" אזלו.
הגדירו עוד תוויות בהודעת המערכת <nowiki>[[MediaWiki:$2]]</nowiki>.',
	'cite_error_references_no_text' => 'תגית <code>&lt;ref&gt;</code> שגויה; לא נכתב טקסט עבור הערת השוליים בשם <code>$1</code>',
	'cite_error_included_ref' => 'חסרה תגית &lt;/ref&gt; המתאימה לתגית &lt;ref&gt;',
	'cite_error_refs_without_references' => 'קיימות תגיות <code>&lt;ref&gt;</code>, אך לא נמצאה תגית <code>&lt;references/&gt;</code>',
	'cite_error_group_refs_without_references' => 'קיימות תגיות <code>&lt;ref&gt;</code> עבור קבוצה הנקראת "$1", אך לא נמצאה תגית <code>&lt;references group="$1"/&gt;</code> מתאימה',
	'cite_error_references_group_mismatch' => 'לתגית <code>&lt;ref&gt;</code> המוגדרת בתוך <code>&lt;references&gt;</code> יש מאפיין קבוצה סותר, "$1".',
	'cite_error_references_missing_group' => 'לתגית <code>&lt;ref&gt;</code> המוגדרת בתוך <code>&lt;references&gt;</code> יש מאפיין קבוצה "$1", שאינו מופיע בטקסט שלפניה.',
	'cite_error_references_missing_key' => 'התגית <code>&lt;ref&gt;</code> בשם "$1" המוגדרת בתוך <code>&lt;references&gt;</code> אינה נמצאת בשימוש בטקסט שלפניה.',
	'cite_error_references_no_key' => 'לתגית <code>&lt;ref&gt;</code> המוגדרת בתוך <code>&lt;references&gt;</code> אין מאפיין name.',
	'cite_error_empty_references_define' => 'התגית <code>&lt;ref&gt;</code> בשם "$1" המוגדרת בתוך <code>&lt;references&gt;</code> אינה מכילה תוכן.',
);

/** Hindi (हिन्दी)
 * @author Kaustubh
 */
$messages['hi'] = array(
	'cite-desc' => '<nowiki><ref[ name=id]></nowiki> और <nowiki><references/></nowiki> यह दो संदर्भ देनेके लिये इस्तेमालमें आने वाले शब्द बढाये जायेंगे।',
	'cite_croak' => 'संदर्भ दे नहीं पाये; $1: $2',
	'cite_error_key_str_invalid' => 'आंतर्गत गलती;
गलत $str या/और $key।
ऐसा होना नहीं चाहियें।',
	'cite_error_stack_invalid_input' => 'आंतर्गत गलती; गलत स्टॅक की। ऐसा होना नहीं चाहियें।',
	'cite_error' => 'गलती उद्घृत करें: $1',
	'cite_error_ref_numeric_key' => '<code>&lt;ref&gt;</code> गलत कोड; नाम यह पूर्णांकी संख्या नहीं हो सकता, कृपया माहितीपूर्ण शीर्षक दें',
	'cite_error_ref_no_key' => '<code>&lt;ref&gt;</code> गलत कोड; खाली संदर्भोंको नाम होना आवश्यक हैं',
	'cite_error_ref_too_many_keys' => '<code>&lt;ref&gt;</code> गलत कोड; गलत नाम, उदा. ढेर सारी',
	'cite_error_ref_no_input' => '<code>&lt;ref&gt;</code> गलत कोड; नाम ना होने वाले संदर्भोंमें ज़ानकारी देना आवश्यक हैं',
	'cite_error_references_invalid_parameters' => '<code>&lt;references&gt;</code> चुकीचा कोड; पॅरॅमीटर्स नहीं दे सकते, <code>&lt;references /&gt;</code> का इस्तेमाल करें',
	'cite_error_references_invalid_parameters_group' => '<code>&lt;references&gt;</code> गलत कोड; सिर्फ पॅरॅमीटर का "ग्रुप" इस्तेमाल में लाया जा सकता हैं, <code>&lt;references /&gt;</code> या फिर <code>&lt;references group="..." /&gt;</code> का इस्तेमाल करें',
	'cite_error_references_no_backlink_label' => 'तैयार किये हुए पीछे की कड़ियां देनेवाले नाम खतम हुए हैं, अधिक नाम <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki> इस संदेश में बढायें',
	'cite_error_references_no_text' => '<code>&lt;ref&gt;</code> गलत कोड; <code>$1</code> नामके संदर्भमें ज़ानकारी नहीं हैं',
);

/** Croatian (Hrvatski)
 * @author Dalibor Bosits
 * @author Dnik
 * @author SpeedyGonsales
 */
$messages['hr'] = array(
	'cite-desc' => 'Dodaje <nowiki><ref[ name=id]></nowiki> i <nowiki><references/></nowiki> oznake, za citiranje',
	'cite_croak' => 'Nevaljan citat; $1: $2',
	'cite_error_key_str_invalid' => 'Unutrašnja greška: loš $str i/ili $key. Ovo se nikada ne bi smjelo dogoditi.',
	'cite_error_stack_invalid_input' => 'Unutrašnja greška; loš ključ stacka.  Ovo se nikada ne bi smjelo dogoditi.',
	'cite_error' => 'Greška u citiranju: $1',
	'cite_error_ref_numeric_key' => 'Loša <code>&lt;ref&gt;</code> oznaka; naziv ne smije biti jednostavni broj, koristite opisni naziv',
	'cite_error_ref_no_key' => 'Loša <code>&lt;ref&gt;</code> oznaka; ref-ovi bez sadržaja moraju imati naziv',
	'cite_error_ref_too_many_keys' => 'Loša <code>&lt;ref&gt;</code> oznaka; loš naziv, npr. previše naziva',
	'cite_error_ref_no_input' => 'Loša <code>&lt;ref&gt;</code> oznaka; ref-ovi bez imena moraju imati sadržaj',
	'cite_error_references_invalid_parameters' => 'Loša <code>&lt;references&gt;</code> oznaka; parametri nisu dozvoljeni, koristite <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Neispravna <code>&lt;references&gt;</code> oznaka;
Dopuštena je samo opcija "group".
Koristite <code>&lt;references /&gt;</code>, ili <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Potrošene sve posebne oznake za poveznice unatrag, definirajte više u poruci <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_no_link_label_group' => 'Nedovoljan broj proizvoljnih naslova poveznica za grupu "$1".
Definirajte više putem poruke <nowiki>[[MediaWiki:$2]]</nowiki>.',
	'cite_error_references_no_text' => 'Nije zadan tekst za izvor <code>$1</code>',
	'cite_error_included_ref' => 'Nedostaje zatvarajući &lt;/ref&gt; za &lt;ref&gt; oznaku',
	'cite_error_refs_without_references' => 'oznake <code>&lt;ref&gt;</code> postoje, ali oznaka <code>&lt;references/&gt;</code> nije pronađena',
	'cite_error_group_refs_without_references' => 'oznake <code>&lt;ref&gt;</code> postoje za skupinu imenovanom "$1", ali nema pripadajuće oznake <code>&lt;references group="$1"/&gt;</code>',
	'cite_error_references_group_mismatch' => '<code>&lt;ref&gt;</code> oznaka u <code>&lt;references&gt;</code> ima konfliktni grupni atribut  "$1".',
	'cite_error_references_missing_group' => '<code>&lt;ref&gt;</code> oznaka definirana u <code>&lt;references&gt;</code> ima grupni atribut "$1" koji se ne pojavljuje u ranijem tekstu.',
	'cite_error_references_missing_key' => '<code>&lt;ref&gt;</code> oznaka s imenom "$1" definirana u <code>&lt;references&gt;</code> nije prethodno rabljena u tekstu.',
	'cite_error_references_no_key' => '<code>&lt;ref&gt;</code> oznaka definirana u <code>&lt;references&gt;</code> nema parametar "name" (ime).',
	'cite_error_empty_references_define' => '<code>&lt;ref&gt;</code> oznaka definirana u <code>&lt;references&gt;</code> s imenom "$1" nema sadržaja.',
);

/** Upper Sorbian (Hornjoserbsce)
 * @author Michawiki
 */
$messages['hsb'] = array(
	'cite-desc' => 'Přidawa taflički <nowiki><ref[ name=id]></nowiki> a <nowiki><references /></nowiki> za žórłowe podaća',
	'cite_croak' => 'Zmylk w referencnym systemje; $1: $2',
	'cite_error_key_str_invalid' => 'Interny zmylk: njepłaćiwy $str a/abo $key. To njeměło ženje wustupić.',
	'cite_error_stack_invalid_input' => 'Interny zmylk; njepłaćiwy kluč staploweho składa. To njeměło ženje wustupić.',
	'cite_error' => 'Referencny zmylk: $1',
	'cite_error_ref_numeric_key' => 'Njepłaćiwe wužiwanje taflički <code>&lt;ref&gt;</code>; "name" njesmě jednora hódnota integer być, wužij wopisowace mjeno.',
	'cite_error_ref_no_key' => 'Njepłaćiwe wužiwanje taflički <code>&lt;ref&gt;</code>; "ref" bjez wobsaha dyrbi mjeno měć.',
	'cite_error_ref_too_many_keys' => 'Njepłaćiwe wužiwanje taflički <code>&lt;ref&gt;</code>; njepłaćiwe mjena, na př. předołho',
	'cite_error_ref_no_input' => 'Njepłaćiwe wužiwanje taflički <code>&lt;ref&gt;</code>; "ref" bjez mjena dyrbi wobsah měć',
	'cite_error_references_invalid_parameters' => 'Njepłaćiwe wužiwanje taflički <code>&lt;references&gt;</code>; žane parametry dowolene, wužij jenož <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Njepłaćiwa taflička <code>&lt;references&gt;</code>;
jenož parameter "group" je dowoleny.
Wužij <code>&lt;references /&gt;</code> abo <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Zwučene etikety wróćowotkazow wućerpjene.
Definuj wjace w powěsći <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_no_link_label_group' => 'Swójske wotkazowe etikety za skupinu "$1" hižo njejsu.
Definuj dalše w zdźělence <nowiki>[[MediaWiki:$2]]</nowiki>.',
	'cite_error_references_no_text' => 'Njepłaćiwa referenca formy <code>&lt;ref&gt;</code>; žadyn tekst za referency z mjenom  <code>$1</code> podaty.',
	'cite_error_included_ref' => 'Kónčny &lt;/ref&gt; za tafličku &lt;ref&gt; faluje',
	'cite_error_refs_without_references' => 'Taflički <code>&lt;ref&gt;</code> ekistuja, ale žana taflička code>&lt;references/&gt;</code> je so namakała',
	'cite_error_group_refs_without_references' => 'Taflički <code>&lt;ref&gt;</code> eksistuja za skupinu z mjenom "$1", ale njeje so wotpowědowaca taflička <code>&lt;references group="$1"/&gt;</code> namakała',
	'cite_error_references_group_mismatch' => 'Taflička <code>&lt;ref&gt;</code> w <code>&lt;references&gt;</code> je ze skupinskim atributom "$1" w konflikće.',
	'cite_error_references_missing_group' => 'Taflička <code>&lt;ref&gt;</code>, kotraž je w <code>&lt;references&gt;</code> definowana, ma skupinski atribut "$1", kotryž so w prjedawšim teksće njejewi.',
	'cite_error_references_missing_key' => 'Taflička <code>&lt;ref&gt;</code> z mjenom "$1", kotraž je w <code>&lt;references&gt;</code> definowana, so w prjedawšim teksće njewužiwa.',
	'cite_error_references_no_key' => 'Taflička <code>&lt;ref&gt;</code>, kotraž je w <code>&lt;references&gt;</code> definowana, mjenowy atribut nima.',
	'cite_error_empty_references_define' => 'Taflička <code>&lt;ref&gt;</code>, kotraž je w <code>&lt;references&gt;</code> z mjenom "$1" definowana, wobsah nima.',
);

/** Haitian (Kreyòl ayisyen)
 * @author Masterches
 */
$messages['ht'] = array(
	'cite-desc' => 'Ajoute baliz sa yo <nowiki><ref[ name=id]></nowiki> epi <nowiki><referans/></nowiki>, pou sitasyon yo.',
	'cite_croak' => 'Sitasyon sa pa bon ; $1 : $2',
	'cite_error_key_str_invalid' => 'Erè nan sistèm an : $str te dwèt parèt.
Erè sa pat janm dwèt rive.',
	'cite_error_stack_invalid_input' => 'Erè nan sistèm an ; 
kle pil an pa bon ditou.
Sa pa te dwe janm rive',
	'cite_error' => 'Erè nan sitasyon : $1',
	'cite_error_ref_numeric_key' => 'Apèl ou fè an pa bon ; se kle ki pa entegral, ki pat long nou tap tann',
	'cite_error_ref_no_key' => 'Apèl sa pa bon : nou pa bay pyès kle',
	'cite_error_ref_too_many_keys' => 'Apèl ou fè an pa bon ; kle yo pa bon, pa egzanp, nou bay twòp kle oubyen kle yo pa bon oubyen nou pa byen rantre yo nan sistèm an.',
	'cite_error_ref_no_input' => 'Apèl ou fè an pa bon ; ou pa presize pyès antre',
	'cite_error_references_invalid_parameters' => 'Agiman, paramèt sa yo pa bon ; agiman nou tap tann',
	'cite_error_references_invalid_parameters_group' => 'Kòd sa pa bon <code>&lt;referans&gt;</code> ;

sèl paramèt ki otorize se « group » (gwoup).

Itilize <code>&lt;references /&gt;</code>, oubyen <code>&lt;references group="..." /&gt;</code> pito.',
	'cite_error_references_no_backlink_label' => 'Pa genyen etikèt, labèl pèsonalize ankò, presize yon nonm, yon kantite ki ta pli gran pase sa ou te bay pli bonè nan mesaj ou an <nowiki>[[MediaWiki:Cite_references_link_many_format_backlink_labels]]</nowiki>',
	'cite_error_references_no_text' => 'Baliz <code>&lt;ref&gt;</code> sa pa bon;
Nou pa bay pyès tèks pou referans nou nonmen yo <code>$1</code>',
);

/** Hungarian (Magyar)
 * @author Dani
 * @author Glanthor Reviol
 * @author KossuthRad
 */
$messages['hu'] = array(
	'cite-desc' => 'Lehetővé teszi idézések létrehozását <nowiki><ref[ name=id]></nowiki> és <nowiki><references/></nowiki> tagek segítségével',
	'cite_croak' => 'Sikertelen forráshivatkozás; $1: $2',
	'cite_error_key_str_invalid' => 'Belső hiba; érvénytelen $str és/vagy $key. Ennek soha nem kellene előfordulnia.',
	'cite_error_stack_invalid_input' => 'Belső hiba; érvénytelen kulcs. Ennek soha nem kellene előfordulnia.',
	'cite_error' => 'Hiba a forráshivatkozásban: $1',
	'cite_error_ref_numeric_key' => 'Érvénytelen <code>&lt;ref&gt;</code> tag; a name értéke nem lehet csupán egy szám, használj leíró címeket',
	'cite_error_ref_no_key' => 'Érvénytelen <code>&lt;ref&gt;</code> tag; a tartalom nélküli ref-eknek kötelező nevet (name) adni',
	'cite_error_ref_too_many_keys' => 'Érvénytelen <code>&lt;ref&gt;</code> tag; hibás nevek, pl. túl sok',
	'cite_error_ref_no_input' => 'Érvénytelen <code>&lt;ref&gt;</code> tag; a név (name) nélküli ref-eknek adni kell valamilyen tartalmat',
	'cite_error_references_invalid_parameters' => 'Érvénytelen <code>&lt;references&gt;</code> tag; nincsenek paraméterei, használd a <code>&lt;references /&gt;</code> formát',
	'cite_error_references_invalid_parameters_group' => 'Érvénytelen <code>&lt;references&gt;</code> tag; csak a „group” attribútum használható. Használd a <code>&lt;references /&gt;</code>, vagy a <code>&lt;references group="..." /&gt;</code> formát.',
	'cite_error_references_no_backlink_label' => 'Elfogytak a visszahivatkozásra használt címkék, adj meg többet a <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki> üzenetben',
	'cite_error_no_link_label_group' => 'Nincs több egyedi címke a következő csoport számára: „$1”.
Definiálj többet a <nowiki>[[MediaWiki:$2]]</nowiki> lapon.',
	'cite_error_references_no_text' => 'Érvénytelen <code>&lt;ref&gt;</code> tag; nincs megadva szöveg a(z) <code>$1</code> nevű ref-eknek',
	'cite_error_included_ref' => 'Egy &lt;ref&gt; tag lezáró &lt;/ref&gt; része hiányzik',
	'cite_error_refs_without_references' => '<code>&lt;ref&gt;</code>-ek vannak a lapon, de nincsen <code>&lt;references/&gt;</code>',
	'cite_error_group_refs_without_references' => '<code>&lt;ref&gt;</code>-ek léteznek a(z) „$1” csoporthoz, de nincs hozzá <code>&lt;references group="$1"/&gt;</code>',
	'cite_error_references_group_mismatch' => 'A <code>&lt;references&gt;</code> és a benne található <code>&lt;ref&gt;</code> tag csoport-attribútuma („$1”) nem egyezik meg.',
	'cite_error_references_missing_group' => '<code>&lt;ref&gt;</code> tag lett lett definiálva egy olyan <code>&lt;references&gt;</code> tagben, amely csoport-attribútuma („$1”) nem szerepel a szöveg korábbi részében.',
	'cite_error_references_missing_key' => 'a <code>&lt;references&gt;</code> tagben definiált „$1” nevű <code>&lt;ref&gt;</code> tag nem szerepel a szöveg korábbi részében.',
	'cite_error_references_no_key' => 'a <code>&lt;references&gt;</code> tagben definiált <code>&lt;ref&gt;</code> tagnek nincs név attribútuma.',
	'cite_error_empty_references_define' => 'a <code>&lt;references&gt;</code> szakaszban definiált „$1” <code>&lt;ref&gt;</code> tagnek nincs tartalma.',
	'cite_references_link_many' => '<li id="$1">^ $2 $3</li>',
);

/** Interlingua (Interlingua)
 * @author McDutchie
 */
$messages['ia'] = array(
	'cite-desc' => 'Adde etiquettas <nowiki><ref[ name=id]></nowiki> e <nowiki><references/></nowiki>, pro citationes',
	'cite_croak' => 'Citation corrumpite; $1: $2',
	'cite_error_key_str_invalid' => 'Error interne;
clave $str e/o $key invalide.
Isto non deberea jammais occurrer.',
	'cite_error_stack_invalid_input' => 'Error interne;
clave de pila invalide.
Isto non deberea jammais occurrer.',
	'cite_error' => 'Error de citation: $1',
	'cite_error_ref_numeric_key' => 'Etiquetta <code>&lt;ref&gt;</code> invalide;
le nomine non pote esser un numero integre. Usa un titulo descriptive',
	'cite_error_ref_no_key' => 'Etiquetta <code>&lt;ref&gt;</code> invalide;
le refs sin contento debe haber un nomine',
	'cite_error_ref_too_many_keys' => 'Etiquetta <code>&lt;ref&gt;</code> invalide;
nomines invalide, p.ex. troppo de nomines',
	'cite_error_ref_no_input' => 'Etiquetta <code>&lt;ref&gt;</code> invalide;
le refs sin nomine debe haber contento',
	'cite_error_references_invalid_parameters' => 'Etiquetta <code>&lt;references&gt;</code> invalide;
nulle parametros es permittite.
Usa <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Etiquetta <code>&lt;references&gt;</code> invalide;
solmente le parametro "group" es permittite.
Usa <code>&lt;references /&gt;</code>, o <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Le etiquettas de retroligamine personalisate es exhaurite.
Defini plus in le message <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_no_link_label_group' => 'Exhauriva le etiquettas de ligamine personalisabile pro le gruppo "$1".
Defini plus de istes in le message <nowiki>[[MediaWiki:$2]]</nowiki>.',
	'cite_error_references_no_text' => 'Etiquetta <code>&lt;ref&gt;</code> invalide;
nulle texto esseva fornite pro le refs nominate <code>$1</code>',
	'cite_error_included_ref' => 'Le clausura &lt;/ref&gt; manca pro le etiquetta &lt;ref&gt;',
	'cite_error_refs_without_references' => 'Il existe etiquettas <code>&lt;ref&gt;</code>, ma nulle etiquetta <code>&lt;references/&gt;</code> ha essite trovate',
	'cite_error_group_refs_without_references' => 'Il existe etiquettas <code>&lt;ref&gt;</code> pro un gruppo nominate "$1", ma nulle etiquetta <code>&lt;references group="$1"/&gt;</code> correspondente ha essite trovate',
	'cite_error_references_group_mismatch' => 'Le etiquetta <code>&lt;ref&gt;</code> in <code>&lt;references&gt;</code> ha un attributo de gruppo "$1" confligente.',
	'cite_error_references_missing_group' => 'Le etiquetta <code>&lt;ref&gt;</code> definite in <code>&lt;references&gt;</code> ha un attributo de gruppo "$1" que non appare in le texto precedente.',
	'cite_error_references_missing_key' => 'Le etiquetta <code>&lt;ref&gt;</code> con nomine "$1" definite in <code>&lt;references&gt;</code> non es usate in le texto precedente.',
	'cite_error_references_no_key' => 'Le etiquetta <code>&lt;ref&gt;</code> definite in <code>&lt;references&gt;</code> non ha un attributo de nomine.',
	'cite_error_empty_references_define' => 'Le etiquetta <code>&lt;ref&gt;</code> definite in <code>&lt;references&gt;</code> con nomine "$1" ha nulle contento.',
);

/** Indonesian (Bahasa Indonesia)
 * @author Bennylin
 * @author Irwangatot
 * @author IvanLanin
 * @author Rex
 */
$messages['id'] = array(
	'cite-desc' => 'Menambahkan tag <nowiki><ref[ name=id]></nowiki> dan <nowiki><references/></nowiki> untuk kutipan',
	'cite_croak' => 'Kegagalan pengutipan; $1: $2',
	'cite_error_key_str_invalid' => 'Kesalahan internal;
$str dan/atau $key tidak sah.
Kesalahan ini seharusnya tidak terjadi.',
	'cite_error_stack_invalid_input' => "Kesalahan internal; 
kunci ''stack'' tak sah.
Kesalahan ini seharusnya tidak terjadi.",
	'cite_error' => 'Kesalahan pengutipan: $1',
	'cite_error_ref_numeric_key' => 'Tag <code>&lt;ref&gt;</code> tidak sah; 
nama tidak boleh intejer sederhana.
Gunakan nama deskriptif',
	'cite_error_ref_no_key' => 'Tag <code>&lt;ref&gt;</code> tidak sah;
referensi tanpa isi harus memiliki nama',
	'cite_error_ref_too_many_keys' => 'Tag <code>&lt;ref&gt;</code> tidak sah;
nama tidak sah; misalnya, terlalu banyak',
	'cite_error_ref_no_input' => 'Tag <code>&lt;ref&gt;</code> tidak sah;
referensi tanpa nama harus memiliki isi',
	'cite_error_references_invalid_parameters' => 'Tag <code>&lt;references&gt;</code> tidak sah;
parameter tidak diperbolehkan.
Gunakan <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Tag <code>&lt;references&gt;</code> tidak sah;
hanya parameter "group" yang diizinkan.
Gunakan <code>&lt;references /&gt;</code>, atau <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Kehabisan label pralana balik tersuai.
Tambahkan lagi di pesan sistem <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_no_link_label_group' => 'Pranala  kustom label untuk kelompok "$1" habis.
Tambahkan ketentuan dalam pesan <nowiki> [[MediaWiki:$2]] </nowiki> .',
	'cite_error_references_no_text' => 'Tag <code>&lt;ref&gt;</code> tidak sah; 
tidak ditemukan teks untuk ref bernama <code>$1</code>',
	'cite_error_included_ref' => 'Tag &lt;ref&gt; harus ditutup oleh &lt;/ref&gt;',
	'cite_error_refs_without_references' => 'Tag <code>&lt;ref&gt;</code> ditemukan, tapi tag <code>&lt;references/&gt;</code> tidak ditemukan',
	'cite_error_group_refs_without_references' => 'Ditemukan tag <code>&lt;ref&gt;</code> untuk kelompok bernama "$1", tapi tidak ditemukan tag <code>&lt;references group="$1"/&gt;</code> yang berkaitan',
	'cite_error_references_group_mismatch' => 'Tag <code>&lt;ref&gt;</code> di <code>&lt;references&gt;</code> memiliki atribut kelompok yang bertentangan "$1".',
	'cite_error_references_missing_group' => 'Tag <code>&lt;ref&gt;</code> yang didefinisikan di <code>&lt;references&gt;</code> memiliki atribut kelompok "$1" yang tidak ditampilkan di teks sebelumnya.',
	'cite_error_references_missing_key' => 'Tag <code>&lt;ref&gt;</code> dengan nama "$1" yang didefinisikan di <code>&lt;references&gt;</code> tidak digunakan pada teks sebelumnya.',
	'cite_error_references_no_key' => 'Tag <code>&lt;ref&gt;</code> yang didefinisikan di di <code>&lt;references&gt;</code> tidak memiliki nama atribut.',
	'cite_error_empty_references_define' => 'Tag <code>&lt;ref&gt;</code> yang didefinisikan di di <code>&lt;references&gt;</code> dengan nama "$1" tidak memiliki isi.',
);

/** Igbo (Igbo)
 * @author Ukabia
 */
$messages['ig'] = array(
	'cite-desc' => 'Tikwá <nowiki><ref[ áhà=id]></nowiki> and <nowiki><references/></nowiki> ndö, maka ntabi okwu',
	'cite_croak' => 'Nchápụ nwụrụ; $1: $2',
);

/** Ido (Ido)
 * @author Malafaya
 */
$messages['io'] = array(
	'cite_croak' => 'Cite mortis; $1: $2',
	'cite_error' => 'Citala eroro: $1',
);

/** Italian (Italiano)
 * @author Beta16
 * @author BrokenArrow
 * @author Darth Kule
 * @author Pietrodn
 */
$messages['it'] = array(
	'cite-desc' => 'Aggiunge i tag <nowiki><ref[ name=id]></nowiki> e <nowiki><references/></nowiki> per gestire le citazioni',
	'cite_croak' => 'Errore nella citazione: $1: $2',
	'cite_error_key_str_invalid' => 'Errore interno;
$str e/o $key errati.
Non dovrebbe mai verificarsi.',
	'cite_error_stack_invalid_input' => 'Errore interno: 
chiave di stack errata.
Non dovrebbe mai verificarsi.',
	'cite_error' => 'Errore nella funzione Cite: $1',
	'cite_error_ref_numeric_key' => "Errore nell'uso del marcatore <code>&lt;ref&gt;</code>: il nome non può essere un numero intero. Usare un titolo esteso",
	'cite_error_ref_no_key' => "Errore nell'uso del marcatore <code>&lt;ref&gt;</code>: i ref vuoti non possono essere privi di nome",
	'cite_error_ref_too_many_keys' => "Errore nell'uso del marcatore <code>&lt;ref&gt;</code>: nomi non validi (ad es. numero troppo elevato)",
	'cite_error_ref_no_input' => "Errore nell'uso del marcatore <code>&lt;ref&gt;</code>: i ref privi di nome non possono essere vuoti",
	'cite_error_references_invalid_parameters' => "Errore nell'uso del marcatore <code>&lt;references&gt;</code>: parametri non ammessi, usare il marcatore <code>&lt;references /&gt;</code>",
	'cite_error_references_invalid_parameters_group' => 'Errore nell\'uso del marcatore <code>&lt;references&gt;</code>;
solo il parametro "group" è permesso.
Usare <code>&lt;references /&gt;</code> oppure <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Etichette di rimando personalizzate esaurite, aumentarne il numero nel messaggio <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_no_link_label_group' => 'Etichette esaurite per collegamenti personalizzati del gruppo "$1", aumentarne il numero nel messaggio <nowiki>[[MediaWiki:$2]]</nowiki>',
	'cite_error_references_no_text' => 'Marcatore <code>&lt;ref&gt;</code> non valido; non è stato indicato alcun testo per il marcatore <code>$1</code>',
	'cite_error_included_ref' => '&lt;/ref&gt; di chiusura mancante per il marcatore &lt;ref&gt;',
	'cite_error_refs_without_references' => 'Sono presenti dei marcatori <code>&lt;ref&gt;</code> ma non è stato trovato alcun marcatore <code>&lt;references/&gt;</code>',
	'cite_error_group_refs_without_references' => 'Sono presenti dei marcatori <code>&lt;ref&gt;</code> per un gruppo chiamato "$1" ma non è stato trovato alcun marcatore <code>&lt;references group="$1"/&gt;</code> corrispondente',
	'cite_error_references_group_mismatch' => 'Il tag <code>&lt;ref&gt;</code> in <code>&lt;references&gt;</code> ha attributo gruppo "$1" in conflitto.',
	'cite_error_references_missing_group' => 'Il tag <code>&lt;ref&gt;</code> definito in <code>&lt;references&gt;</code> ha un attributo gruppo "$1" che non compare nel testo precedente.',
	'cite_error_references_missing_key' => 'Il tag <code>&lt;ref&gt;</code> con nome "$1" definito in <code>&lt;references&gt;</code> non è usato nel testo precedente.',
	'cite_error_references_no_key' => 'Il tag <code>&lt;ref&gt;</code> definito in <code>&lt;references&gt;</code> non ha un attributo nome.',
	'cite_error_empty_references_define' => 'Il tag <code>&lt;ref&gt;</code> definito in <code>&lt;references&gt;</code> con nome "$1" non ha alcun contenuto.',
);

/** Japanese (日本語)
 * @author Aotake
 * @author Ficell
 * @author Fryed-peach
 * @author JtFuruhata
 */
$messages['ja'] = array(
	'cite-desc' => '引用のためのタグ<nowiki><ref[ name=id]></nowiki> および <nowiki><references/></nowiki> を追加する',
	'cite_croak' => '引用タグ機能の重大なエラー。$1: $2',
	'cite_error_key_str_invalid' => '内部エラー。$str と $key の両方または一方が無効。これはソフトウェアのバグです。',
	'cite_error_stack_invalid_input' => '内部エラー。スタックキーが無効。これはソフトウェアのバグです。',
	'cite_error' => '引用エラー: $1',
	'cite_error_ref_numeric_key' => '無効な <code>&lt;ref&gt;</code> タグ。名前(<code>name</code> 属性)に単なる数値は使用できません。説明的なものにしてください',
	'cite_error_ref_no_key' => '無効な <code>&lt;ref&gt;</code> タグ。引用句の内容がない場合には名前(<code>name</code> 属性)が必要です',
	'cite_error_ref_too_many_keys' => '無効な <code>&lt;ref&gt;</code> タグ。引数が不正(数が多すぎる、など)',
	'cite_error_ref_no_input' => '無効な <code>&lt;ref&gt;</code> タグ。名前(<code>name</code> 属性)がない場合には引用句の内容が必要です',
	'cite_error_references_invalid_parameters' => '無効な <code>&lt;references&gt;</code> タグ。引数は指定できません。<code>&lt;references /&gt;</code> を用いてください',
	'cite_error_references_invalid_parameters_group' => '無効な <code>&lt;references&gt;</code> タグです。使用できるパラメータは "group" のみです。<code>&lt;references /&gt;</code> または <code>&lt;references group="..." /&gt;</code> を用いてください',
	'cite_error_references_no_backlink_label' => 'バックリンクラベルが使用できる個数を超えました。<nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki> メッセージでの定義を増やしてください',
	'cite_error_no_link_label_group' => 'グループ「$1」用のカスタム・リンクラベルを使い果たしました。<nowiki>[[MediaWiki:$2]]</nowiki> メッセージを編集してラベルの定義を増やしてください。',
	'cite_error_references_no_text' => '無効な <code>&lt;ref&gt;</code> タグ。「<code>$1</code>」という名前の引用句に対するテキストがありません',
	'cite_error_included_ref' => '&lt;ref&gt; タグに対応する &lt;/ref&gt; が不足しています',
	'cite_error_refs_without_references' => '<code>&lt;ref&gt;</code> タグがありますが、<code>&lt;references/&gt;</code> タグが見つかりません',
	'cite_error_group_refs_without_references' => '「$1」というグループの <code>&lt;ref&gt;</code> タグがありますが、対応する <code>&lt;references group="$1"/&gt;</code> タグが見つかりません',
	'cite_error_references_group_mismatch' => '<code>&lt;references&gt;</code> の<code>&lt;ref&gt;</code> タグに重複するグループ用引数 "$1" が使用されています。',
	'cite_error_references_missing_group' => '<code>&lt;references&gt;</code> で定義されている <code>&lt;ref&gt;</code> タグに使用されているグループ用引数 "$1" は先行するテキスト内に使用されていません。',
	'cite_error_references_missing_key' => '<code>&lt;references&gt;</code> で定義されている "name=$1" を持つ <code>&lt;ref&gt;</code> タグは先行するテキスト内で使用されていません。',
	'cite_error_references_no_key' => '<code>&lt;references&gt;</code> で定義されている <code>&lt;ref&gt;</code> タグには name 属性がありません。',
	'cite_error_empty_references_define' => '<code>&lt;references&gt;</code> で定義されている "name=$1" を持つ <code>&lt;ref&gt;</code> タグには中身がありません。',
);

/** Jutish (Jysk)
 * @author Huslåke
 */
$messages['jut'] = array(
	'cite_croak' => 'Æ fodnåt døde; $1: $2',
	'cite_error_key_str_invalid' => 'Intern fejl: Ugyldeg $str og/æller $key. Dette burde aldreg førekåm.',
	'cite_error_stack_invalid_input' => 'Intern fejl: Ugyldeg staknøgle. Dette burde aldreg førekåm.',
	'cite_error' => 'Fodnåtfejl: $1',
	'cite_error_ref_numeric_key' => 'Ugyldigt <code>&lt;ref&gt;</code>-tag; "name" kan ikke være et simpelt heltal, brug en beskrivende titel',
	'cite_error_ref_no_key' => 'Ugyldigt <code>&lt;ref&gt;</code>-tag: Et <code>&lt;ref&gt;</code>-tag uden indhold skal have et navn',
	'cite_error_ref_too_many_keys' => 'Ugyldigt <code>&lt;ref&gt;</code>-tag: Ugyldege navne, fx før mange',
	'cite_error_ref_no_input' => 'Ugyldigt <code>&lt;ref&gt;</code>-tag: Et <code>&lt;ref&gt;</code>-tag uden navn skal have indhold',
	'cite_error_references_invalid_parameters' => 'Ugyldig <code>&lt;references&gt;</code>-tag: Parametre er ikke tilladt, brug i stedet <code>&lt;references /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'For mange <code>&lt;ref&gt;</code>-tags har det samme "name", tillad flere i beskeden <nowiki>[[MediaWiki:Cite_references_link_many_format_backlink_labels]]</nowiki>',
	'cite_error_references_no_text' => 'Ugyldigt <code>&lt;ref&gt;</code>-tag: Der er ikke specificeret nogen fodnotetekst til navnet <code>$1</code>',
);

/** Javanese (Basa Jawa)
 * @author Meursault2004
 * @author Pras
 */
$messages['jv'] = array(
	'cite-desc' => 'Nambahaké tag <nowiki><ref[ name=id]></nowiki> lan <nowiki><references/></nowiki> kanggo kutipan (sitat)',
	'cite_croak' => 'Sitaté (pangutipané) gagal; $1: $2',
	'cite_error_key_str_invalid' => 'Kaluputan jero;
$str lan/utawa $key ora absah.
Iki sajatiné ora tau olèh kadadéyan.',
	'cite_error_stack_invalid_input' => 'Kaluputan internal;
stack key ora absah.
Iki samesthine ora kadadéan.',
	'cite_error' => 'Kaluputan sitat (pangutipan) $1',
	'cite_error_ref_numeric_key' => 'Tag <code>&lt;ref&gt;</code> ora absah;
jenengé ora bisa namung angka integer waé. Gunakna irah-irahan (judhul) dèskriptif',
	'cite_error_ref_no_key' => 'Tag <code>&lt;ref&gt;</code> ora absah;
refs tanpa isi kudu duwé jeneng',
	'cite_error_ref_too_many_keys' => 'Tag <code>&lt;ref&gt;</code> ora absah;
jeneng-jenengé ora absah, contoné kakèhan',
	'cite_error_ref_no_input' => 'Tag <code>&lt;ref&gt;</code> ora absah;
refs tanpa jeneng kudu ana isiné',
	'cite_error_references_invalid_parameters' => 'Tag <code>&lt;references&gt;</code> ora absah;
ora ana paramèter sing diidinaké.
Gunakna <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Tag <code>&lt;references&gt;</code> ora absah;
namung paramèter "group" sing diolèhaké.
Gunakna <code>&lt;references /&gt;</code>, utawa <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Kentèkan label pranala balik.
Tambahna ing pesenan sistém <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_references_no_text' => 'Tag <code>&lt;ref&gt;</code> ora absah; 
ora ditemokaké tèks kanggo ref mawa jeneng <code>$1</code>',
	'cite_error_included_ref' => 'Tag &lt;ref&gt; kudu ditutup déning &lt;/ref&gt;',
);

/** Georgian (ქართული)
 * @author გიორგიმელა
 */
$messages['ka'] = array(
	'cite-desc' => 'ამატებს <nowiki><ref[ name=id]></nowiki> და <nowiki><references/></nowiki> ტეგებს სქოლიოსთვის',
	'cite_croak' => 'ციტატა მოკვადა; $1: $2',
	'cite_error_key_str_invalid' => 'შიდა შეცდომა
არასწორი $str და/ან $key
ასეთი არასდროს არ უნდა განმეორდეს',
	'cite_error_stack_invalid_input' => 'შიდა შეცდომა.
სტეკის არასწორი გასაღები.
ეს არ უნდა განმეორდეს.',
	'cite_error' => 'ციტირების შეცდომა $1',
	'cite_error_ref_numeric_key' => 'არასწორი ტეგი <code>&lt;ref&gt;</code> tag;
სახელმიარ უნდა შეიცავდეს ციფრებს.',
	'cite_error_ref_no_key' => 'არასწორი ტეგი <code>&lt;ref&gt;</code>;
ელემენტი უნდა შეიცავდეს სახელს.',
	'cite_error_ref_too_many_keys' => 'არასწორი ტეგი <code>&lt;ref&gt;</code>;
არასწორი სახელები, ძალიან ბევრი.',
	'cite_error_ref_no_input' => 'არასწორი ტეგი <ref>; ელემენტი უნდა შეიცავდეს შინაარს.',
);

/** Kazakh (Arabic script) (‫قازاقشا (تٴوتە)‬) */
$messages['kk-arab'] = array(
	'cite_croak' => 'دٵيەكسٶز الۋ سٵتسٸز بٸتتٸ; $1: $2',
	'cite_error_key_str_invalid' => 'ٸشكٸ قاتە; جارامسىز $str',
	'cite_error_stack_invalid_input' => 'ٸشكٸ قاتە; جارامسىز ستەك كٸلتٸ',
	'cite_error' => 'دٵيەكسٶز الۋ $1 قاتەسٸ',
	'cite_error_ref_numeric_key' => 'جارامسىز <code>&lt;ref&gt;</code> بەلگٸشەسٸ; اتاۋ كٵدٸمگٸ بٷتٸن سان بولۋى مٷمكٸن ەمەس, سيپپاتاۋىش اتاۋ قولدانىڭىز',
	'cite_error_ref_no_key' => 'جارامسىز <code>&lt;ref&gt;</code> بەلگٸشەسٸ; ماعلۇماتسىز تٷسٸنٸكتەمەلەردە اتاۋ بولۋى قاجەت',
	'cite_error_ref_too_many_keys' => 'جارامسىز <code>&lt;ref&gt;</code> بەلگٸشە; جارامسىز اتاۋلار, مىسالى, تىم كٶپ',
	'cite_error_ref_no_input' => 'جارامسىز <code>&lt;ref&gt;</code> بەلگٸشە; اتاۋسىز تٷسٸنٸكتەمەلەردە ماعلۇماتى بولۋى قاجەت',
	'cite_error_references_invalid_parameters' => 'جارامسىز <code>&lt;references&gt;</code> بەلگٸشە; ەش باپتار رۇقسات ەتٸلمەيدٸ, بىلاي <code>&lt;references /&gt;</code> قولدانىڭىز',
	'cite_error_references_no_backlink_label' => 'قوسىمشا بەلگٸلەردٸڭ سانى بٸتتٸ, ودان ٵرٸ كٶبٸرەك <nowiki>[[MediaWiki:Cite_references_link_many_format_backlink_labels]]</nowiki> جٷيە حابارىندا بەلگٸلەڭٸز',
);

/** Kazakh (Cyrillic) (Қазақша (Cyrillic)) */
$messages['kk-cyrl'] = array(
	'cite_croak' => 'Дәйексөз алу сәтсіз бітті; $1: $2',
	'cite_error_key_str_invalid' => 'Ішкі қате; жарамсыз $str',
	'cite_error_stack_invalid_input' => 'Ішкі қате; жарамсыз стек кілті',
	'cite_error' => 'Дәйексөз алу $1 қатесі',
	'cite_error_ref_numeric_key' => 'Жарамсыз <code>&lt;ref&gt;</code> белгішесі; атау кәдімгі бүтін сан болуы мүмкін емес, сиппатауыш атау қолданыңыз',
	'cite_error_ref_no_key' => 'Жарамсыз <code>&lt;ref&gt;</code> белгішесі; мағлұматсыз түсініктемелерде атау болуы қажет',
	'cite_error_ref_too_many_keys' => 'Жарамсыз <code>&lt;ref&gt;</code> белгіше; жарамсыз атаулар, мысалы, тым көп',
	'cite_error_ref_no_input' => 'Жарамсыз <code>&lt;ref&gt;</code> белгіше; атаусыз түсініктемелерде мағлұматы болуы қажет',
	'cite_error_references_invalid_parameters' => 'Жарамсыз <code>&lt;references&gt;</code> белгіше; еш баптар рұқсат етілмейді, былай <code>&lt;references /&gt;</code> қолданыңыз',
	'cite_error_references_no_backlink_label' => 'Қосымша белгілердің саны бітті, одан әрі көбірек <nowiki>[[MediaWiki:Cite_references_link_many_format_backlink_labels]]</nowiki> жүйе хабарында белгілеңіз',
);

/** Kazakh (Latin) (Қазақша (Latin)) */
$messages['kk-latn'] = array(
	'cite_croak' => 'Däýeksöz alw sätsiz bitti; $1: $2',
	'cite_error_key_str_invalid' => 'İşki qate; jaramsız $str',
	'cite_error_stack_invalid_input' => 'İşki qate; jaramsız stek kilti',
	'cite_error' => 'Däýeksöz alw $1 qatesi',
	'cite_error_ref_numeric_key' => 'Jaramsız <code>&lt;ref&gt;</code> belgişesi; ataw kädimgi bütin san bolwı mümkin emes, sïppatawış ataw qoldanıñız',
	'cite_error_ref_no_key' => 'Jaramsız <code>&lt;ref&gt;</code> belgişesi; mağlumatsız tüsiniktemelerde ataw bolwı qajet',
	'cite_error_ref_too_many_keys' => 'Jaramsız <code>&lt;ref&gt;</code> belgişe; jaramsız atawlar, mısalı, tım köp',
	'cite_error_ref_no_input' => 'Jaramsız <code>&lt;ref&gt;</code> belgişe; atawsız tüsiniktemelerde mağlumatı bolwı qajet',
	'cite_error_references_invalid_parameters' => 'Jaramsız <code>&lt;references&gt;</code> belgişe; eş baptar ruqsat etilmeýdi, bılaý <code>&lt;references /&gt;</code> qoldanıñız',
	'cite_error_references_no_backlink_label' => 'Qosımşa belgilerdiñ sanı bitti, odan äri köbirek <nowiki>[[MediaWiki:Cite_references_link_many_format_backlink_labels]]</nowiki> jüýe xabarında belgileñiz',
);

/** Khmer (ភាសាខ្មែរ)
 * @author វ័ណថារិទ្ធ
 */
$messages['km'] = array(
	'cite-desc' => 'បន្ថែម ស្លាក <nowiki><ref[ name=id]></nowiki> និង <nowiki><references/></nowiki>​ សម្រាប់ការយោង​ឯកសារ​',
);

/** Korean (한국어)
 * @author Ficell
 * @author Ilovesabbath
 * @author Kwj2772
 * @author ToePeu
 */
$messages['ko'] = array(
	'cite-desc' => '인용에 쓰이는 <nowiki><ref[ name=id]></nowiki>와 <nowiki><references/></nowiki>태그를 더합니다.',
	'cite_croak' => '인용 오류; $1: $2',
	'cite_error_key_str_invalid' => '내부 오류;
$str 혹은 $key가 잘못되었습니다.
이 오류는 발생하지 않아야 합니다.',
	'cite_error_stack_invalid_input' => '내부 오류; 스택 키가 잘못되었습니다.
이 오류는 발생하지 말아야 합니다.',
	'cite_error' => '인용 오류: $1',
	'cite_error_ref_numeric_key' => '<code>&lt;ref&gt;</code> 태그가 잘못되었습니다;
이름은 숫자가 될 수 없습니다. 설명적인 이름을 사용하십시오.',
	'cite_error_ref_no_key' => '<code>&lt;ref&gt;</code> 태그가 잘못되었습니다;
내용이 없는 주석은 이름이 있어야 합니다.',
	'cite_error_ref_too_many_keys' => '잘못된 <code>&lt;ref&gt;</code> 태그 사용;
예컨대 잘못된 주석 이름이 너무 많습니다.',
	'cite_error_ref_no_input' => '<code>&lt;ref&gt;</code> 태그가 잘못되었습니다;
이름이 없는 ref 태그는 반드시 내용이 있어야 합니다.',
	'cite_error_references_invalid_parameters' => '<code>&lt;references&gt;</code> 태그가 잘못되었습니다;
변수를 넣어서는 안 됩니다.
<code>&lt;references /&gt;</code>를 이용하십시오.',
	'cite_error_references_invalid_parameters_group' => '<code>&lt;references&gt;</code> 태그가 잘못되었습니다;
"group" 변수만 사용할 수 있습니다.
<code>&lt;references /&gt;</code>나 <code>&lt;references group="..." /&gt;</code>만 이용하십시오.',
	'cite_error_references_no_backlink_label' => '역링크 라벨이 부족합니다.
<nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>에 더 많은 라벨을 추가하십시오.',
	'cite_error_no_link_label_group' => '그룹 "$1"에 대해 링크 레이블이 모두 떨어졌습니다.
<nowiki>[[MediaWiki:$2]]</nowiki> 메시지에 더 많은 레이블을 정의해주십시오.',
	'cite_error_references_no_text' => '<code>&lt;ref&gt;</code> 태그가 잘못되었습니다.
<code>$1</code>라는 이름을 가진 주석에 대한 내용이 없습니다.',
	'cite_error_included_ref' => '&lt;ref&gt; 태그를 닫는 &lt;/ref&gt; 태그가 없습니다.',
	'cite_error_refs_without_references' => '<code>&lt;ref&gt;</code> 태그가 존재하지만, <code>&lt;references/&gt;</code> 태그가 없습니다.',
	'cite_error_group_refs_without_references' => '"$1"이라는 이름을 가진 그룹에 대한 <code>&lt;ref&gt;</code> 태그가 존재하지만, 이에 대응하는 <code>&lt;references group="$1" /&gt;</code> 태그가 없습니다.',
	'cite_error_references_group_mismatch' => '<code>&lt;references&gt;</code> 안에 있는 <code>&lt;ref&gt;</code> 태그의 그룹 속성 "$1"이 충돌됩니다.',
	'cite_error_references_missing_group' => '<code>&lt;references&gt;</code> 안의 <code>&lt;ref&gt;</code> 태그가 이전에 존재하지 않는 그룹 속성 "$1"을 갖고 있습니다.',
	'cite_error_references_missing_key' => '<code>&lt;references&gt;</code> 안에 정의된 "$1"이라는 이름을 가진 <code>&lt;ref&gt;</code> 태그가 위에서 사용되고 있지 않습니다.',
	'cite_error_references_no_key' => '<code>&lt;references&gt;</code> 안의 <code>&lt;ref&gt;</code> 태그에 이름이 없습니다.',
	'cite_error_empty_references_define' => '<code>&lt;references&gt;</code> 태그 안에 정의된 "$1"이라는 이름을 가진 <code>&lt;ref&gt;</code> 태그에 내용이 없습니다.',
	'cite_references_link_many_format_backlink_labels' => '가 나 다 라 마 바 사 아 자 차 카 타 파 하 거 너 더 러 머 버 서 어 저 처 커 터 퍼 허 고 노 도 로 모 보 소 오 조 초 코 토 포 호 구 누 두 루 무 부 수 우 주 추 쿠 투 푸 후 그 느 드 르 므 브 스 으 즈 츠 크 트 프 흐 기 니 디 리 미 비 시 이 지 치 키 티 피 히',
);

/** Colognian (Ripoarisch)
 * @author Purodha
 * @author The Evil IP address
 */
$messages['ksh'] = array(
	'cite-desc' => 'Erlaub Quelle un Referenze met <nowiki><ref[ name="id"]></nowiki> un <nowiki><references /></nowiki> aanzejevve.',
	'cite_croak' => 'Fääler met Refenenze. $1: $2',
	'cite_error_key_str_invalid' => 'Interne Fähler in <i lang="en">cite</i>:
<code>$str</code> udder <code>$key</code> stemme nit.
Dat sull nie optredde.',
	'cite_error_stack_invalid_input' => 'Interne Fähler in <i lang="en">cite</i>:
Der <i lang="en">stack</i>-Schlößel stemmp nit.
Dat sull nie optredde.',
	'cite_error' => 'Fähler in <i lang="en">cite</i> met Referenze: $1',
	'cite_error_ref_numeric_key' => 'Fähler en <i lang="en">cite</i>:
Ene <code>&lt;ref&gt;</code>-Name kann kei Zahl sin.
Nemm enne Tittel, dä jät säht.',
	'cite_error_ref_no_key' => 'Fähler en <i lang="en">cite</i>:
E <code>&lt;ref&gt;</code> oohne Enhalt moß ene Name han.
Nemm enne Tittel, dä jät säht.',
	'cite_error_ref_too_many_keys' => 'Fähler en <i lang="en">cite</i>:
Zo fill <code>&lt;ref&gt;</code>-Name,
udder kapodde ene Name.',
	'cite_error_ref_no_input' => 'Fähler en <i lang="en">cite</i>:
E <code>&lt;ref&gt;</code> oohne Name moß ene Enhallt han.',
	'cite_error_references_invalid_parameters' => 'Fähler en <i lang="en">cite</i>:
E <code>&lt;references&gt;</code> moß oohne Parrametere sin.
Nemm eifach <code>&lt;references /&gt;</code> un söns nix.',
	'cite_error_references_invalid_parameters_group' => 'Fähler en <i lang="en">cite</i>:
E <code>&lt;references&gt;</code> darf nur dä Parrameeter „<code>group</code>“ han.
Nemm eifach <code>&lt;references /&gt;</code> udder <code>&lt;references group="..." /&gt;</code> un söns nix.',
	'cite_error_references_no_backlink_label' => 'Fähler en <i lang="en">cite</i>:
Nit jenoch Name för retuur-Lengks.
Donn mieh en dä Sigg <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki> enndrare.',
	'cite_error_no_link_label_group' => 'För de Jruppe „$1“ senn er kein Bezeichnunge för Links mieh doh.
Donn op <nowiki>[[MediaWiki:$2]]</nowiki> noch e paa dobei.',
	'cite_error_references_no_text' => 'Fähler en <i lang="en">cite</i>:
Et wohr keine Tex aanjejovve för de
<code>&lt;ref&gt;</code>s met dämm Name „<code>$1</code>“.',
	'cite_error_included_ref' => 'Hee för dat &lt;ref&gt; ham_mer kei zopaß &lt;/ref&gt;',
	'cite_error_refs_without_references' => 'Et sinn_er <code>&lt;ref&gt;</code>-Befähle en dä Sigg, ävver mer han keine <code>&lt;references/&gt;</code>-Befähl jefunge.',
	'cite_error_group_refs_without_references' => 'Et sinn_er <code>&lt;ref&gt;</code>-Befähle för de jrop „$1“ en hee dä Sigg, ävver mer han keine <code>&lt;references group="$1"/&gt;</code>-Befähl jefunge.',
	'cite_error_references_group_mismatch' => 'Dä <code>&lt;ref&gt;</code> Befähl en <code>&lt;references&gt;</code> hät en widerschpröschlesche Jroppe-Eijeschaff „$1“.',
	'cite_error_references_missing_group' => 'Dä <code>&lt;ref&gt;</code> Befähl, aanjejoove em Befähl <code>&lt;references&gt;</code>, hät en Jroppe-Eijeschaff „$1“, di ävver em Täx doför nit vörjekumme es.',
	'cite_error_references_missing_key' => 'Dä <code>&lt;ref&gt;</code> Befähl mem Naame „$1“, aanjejoove em Befähl <code>&lt;references&gt;</code>, es em Täx doför nit vörjekumme.',
	'cite_error_references_no_key' => 'Dä <code>&lt;ref&gt;</code> Befähl, aanjejoove em Befähl <code>&lt;references&gt;</code>, hät kei Eijeschaff <code>name=</code> aanjejovve.',
	'cite_error_empty_references_define' => 'Dä <code>&lt;ref&gt;</code> Befähl mem Naame „$1“, aanjejoove em Befähl <code>&lt;references&gt;</code> mem Name „$1“, hät keine Enhallt.',
	'cite_reference_link_key_with_num' => '$1_$2',
	'cite_reference_link_prefix' => 'fohss_noht_betreck_',
	'cite_references_link_prefix' => 'fohss_noht_nommer_',
	'cite_references_link_many_and' => '&#32;',
);

/** Luxembourgish (Lëtzebuergesch)
 * @author Les Meloures
 * @author Robby
 */
$messages['lb'] = array(
	'cite-desc' => 'Setzt <nowiki><ref[ name=id]></nowiki> an <nowiki><references/></nowiki> Taggen derbäi, fir Zitatiounen.',
	'cite_croak' => 'Feeler am Referenz-System. $1 : $2',
	'cite_error_key_str_invalid' => 'Interne Feeler;
net valabele $str an/oder $key.
Dëst sollt eigentlech ni geschéien.',
	'cite_error_stack_invalid_input' => "Interne Feeler;
ongëltege ''stack''-Schlëssel.
Dës sollt eigentlech guer net geschéien.",
	'cite_error' => 'Zitéierfeeler: $1',
	'cite_error_ref_numeric_key' => 'Ongëltegen <code>&lt;ref&gt;</code> Tag;
Den Numm ka keng einfach ganz Zuel sinn. Benotzt w.e.g. een Titel den eng Beschreiwung gëtt',
	'cite_error_ref_no_key' => 'Ongëltegen <code>&lt;ref&gt;</code> Tag;
Referenzen ouni Inhalt mussen e Numm hunn',
	'cite_error_ref_too_many_keys' => 'Ongëltege <code>&lt;ref&gt;</code> Tag;
ongëlteg Nimm, z. Bsp. zevill',
	'cite_error_ref_no_input' => "Ongëltege <code>&lt;ref&gt;</code> Tag;
''refs'' ouni Numm muss een Inhalt hun",
	'cite_error_references_invalid_parameters' => 'Ongëltegen <code>&lt;references&gt;</code> Tag;
et si keng Parameter erlaabt.
Benotzt <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Ongëltege  <code>&lt;references&gt;</code> Tag;
nëmmen de Parameter "group" ass erlaabt.
Benotzt <code>&lt;references /&gt;</code>, oder <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_text' => "Ongëlteg <code>&lt;ref&gt;</code> Markéierung;
et gouf keen Text ugi fir d'Referenze mam Numm <code>$1</code>",
	'cite_error_included_ref' => 'Den Tag &lt;/ref&gt; feelt fir den Tag &lt;ref&gt; zouzemaachen',
	'cite_error_refs_without_references' => "D'Markéierung <code>&lt;ref&gt;</code> gëtt et, awer d'Markéierung <code>&lt;references/&gt;</code> gouf net fonnt",
	'cite_error_group_refs_without_references' => 'D\'Markéierung <code>&lt;ref&gt;</code> gëtt et fir d\'Grupp "$1", awer d\'entspriechend Markéierung <code>&lt;references group="$1"/&gt;</code> gouf net fonnt',
	'cite_error_references_group_mismatch' => 'Den <code>&lt;ref&gt;</code>-Tag an <code>&lt;references&gt;</code> huet den Attribut "$1" deen am Konflikt mat deem am <code>&lt;references&gt;</code> steet.',
	'cite_error_references_missing_group' => 'Deen am <code>&lt;references&gt;</code> definéierten <code>&lt;ref&gt;</code>-Tag huet en Attribut "$1" deen am Text virdrun net dran ass.',
	'cite_error_references_missing_key' => 'Deen am <code>&lt;references&gt;</code> definéierten <code>&lt;ref&gt;</code>-Tag mam Numm "$1" gëtt am Text virdrun net benotzt.',
	'cite_error_references_no_key' => "D'Markéierung <code>&lt;ref&gt;</code> déi an <code>&lt;references&gt;</code> definéiert ass huet keng Nummeegeschaft.",
	'cite_error_empty_references_define' => "D'Markéierung <code>&lt;ref&gt;</code> déi am <code>&lt;references&gt;</code> mat dem Numm  « $1 » definéiert ass, ass eidel.",
);

/** Limburgish (Limburgs)
 * @author Ooswesthoesbes
 * @author Pahles
 */
$messages['li'] = array(
	'cite-desc' => 'Voeg <nowiki><ref[ name=id]></nowiki> en <nowiki><references/></nowiki> tags toe veur citate',
	'cite_croak' => 'Perbleem mit Citere; $1: $2',
	'cite_error_key_str_invalid' => 'Interne fout; ónzjuuste $str en/of $key.  Dit zów noeaits mótte veurkómme.',
	'cite_error_stack_invalid_input' => 'Interne fout; ónzjuuste stacksleutel.  Dit zów noeaits mótte veurkómme.',
	'cite_error' => 'Citeerfout: $1',
	'cite_error_ref_numeric_key' => "Ónzjuuste tag <code>&lt;ref&gt;</code>; de naam kin gein simpele integer zeen, gebroek 'ne besjrievendje titel",
	'cite_error_ref_no_key' => "Ónzjuuste tag <code>&lt;ref&gt;</code>; refs zónger inhoud mótte 'ne naam höbbe",
	'cite_error_ref_too_many_keys' => 'Ónzjuuste tag <code>&lt;ref&gt;</code>; ónzjuuste name, beveurbeildj te väöl',
	'cite_error_ref_no_input' => 'Ónzjuuste tag <code>&lt;ref&gt;</code>; refs zónger naam mótte inhoud höbbe',
	'cite_error_references_invalid_parameters' => 'Ónzjuuste tag <code>&lt;references&gt;</code>; paramaeters zeen neet toegestaon, gebroek <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Onjuuste tag <code>&lt;references&gt;</code>;
allein de paramaeter "group" is toegestaon.
Gebruik <code>&lt;references /&gt;</code>, of <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => "'t Aantal besjikbare backlinklabels is opgebroek. Gaef meer labels op in 't berich <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>",
	'cite_error_references_no_text' => "Ónzjuuste tag <code>&lt;ref&gt;</code>; d'r is gein teks opgegaeve veur refs mit de naam <code>$1</code>",
	'cite_error_included_ref' => 'Gein sjloetteike &lt;/ref&gt; veur de tag &lt;ref&gt;',
	'cite_error_refs_without_references' => 'De tag <code>&lt;ref&gt;</code> besteit al, meh de tag <code>&lt;references/&gt;</code> is neet aangetróffe',
	'cite_error_group_refs_without_references' => 'd\'r Besteit \'ne tag <code>&lt;ref&gt;</code> veure groep "$1", meh d\'r is geine bebehuuerendje tag <code>&lt;references group="$1"/&gt;</code> gevónje',
);

/** Lithuanian (Lietuvių)
 * @author Homo
 * @author Matasg
 */
$messages['lt'] = array(
	'cite_croak' => 'Cituoti nepavyko; $1: $2',
	'cite_error_key_str_invalid' => 'Vidinė klaida; neleistinas $str',
	'cite_error_stack_invalid_input' => 'Vidinė klaida; neleistinas steko raktas',
	'cite_error' => 'Citavimo klaida $1',
	'cite_error_ref_numeric_key' => 'Neleistina <code>&lt;ref&gt;</code> gairė; vardas negali būti tiesiog skaičius, naudokite tekstinį pavadinimą',
	'cite_error_ref_no_key' => 'Neleistina <code>&lt;ref&gt;</code> gairė; nuorodos be turinio turi turėti vardą',
	'cite_error_ref_too_many_keys' => 'Neleistina <code>&lt;ref&gt;</code> gairė; neleistini vardai, pvz., per daug',
	'cite_error_ref_no_input' => 'Neleistina <code>&lt;ref&gt;</code> gairė; nuorodos be vardo turi turėti turinį',
	'cite_error_references_invalid_parameters' => 'Neleistina <code>&lt;references&gt;</code> gairė; neleidžiami jokie parametrai, naudokite <code>&lt;references /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Baigėsi antraštės.
Nurodykite daugiau <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki> sisteminiame tekste',
);

/** Latvian (Latviešu)
 * @author GreenZeb
 * @author Marozols
 * @author Xil
 */
$messages['lv'] = array(
	'cite-desc' => 'Pievieno <nowiki><ref[ name=id]></nowiki> un <nowiki><references/></nowiki> tagus, atsaucēm',
	'cite_error' => 'Kļūda atsaucē: $1',
	'cite_error_refs_without_references' => 'atrasta <code>&lt;ref&gt;</code> iezīme, bet nav nevienas <code>&lt;references/&gt;</code> iezīmes',
);

/** Malagasy (Malagasy)
 * @author Jagwar
 */
$messages['mg'] = array(
	'cite-desc' => 'Mamnpy ny balizy <tt><nowiki><ref[ name="id"]></nowiki></tt> et <tt><nowiki><references/></nowiki></tt> ho an\'ny tsiahy.',
	'cite_croak' => 'Tsiahy tsy miafana ; $1 : $2',
	'cite_error_key_str_invalid' => 'Tsy fetezana ety anaty;
Tsy mety $str na $key.
Tokony tsy hitranga mihintsy ity tsy fetezana ity.',
	'cite_error_stack_invalid_input' => 'Tsy fetezana ety anaty ;
tsy mety ny stack key.
Tokony tsy hitranga mihitsy ity tsy fetezana ity.',
);

/** Macedonian (Македонски)
 * @author Bjankuloski06
 * @author Brest
 */
$messages['mk'] = array(
	'cite-desc' => 'Додава <nowiki><ref[ name=id]></nowiki> и <nowiki><references/></nowiki> ознаки, за цитирања',
	'cite_croak' => 'Непостоечки цитат; $1: $2',
	'cite_error_key_str_invalid' => 'Интерна грешка;
погрешен $str и/или $key.
Ова не треба никогаш да се појави.',
	'cite_error_stack_invalid_input' => 'Интерна грешка;
погрешен клуч за купот.
Ова не треба никогаш да се појави.',
	'cite_error' => 'Грешка во цитирање: $1.',
	'cite_error_ref_numeric_key' => 'Погрешна <code>&lt;ref&gt;</code> ознака;
името не може да биде број. Употребувајте описен наслов',
	'cite_error_ref_no_key' => 'Погрешна <code>&lt;ref&gt;</code> ознака;
навод без содржина мора да има име',
	'cite_error_ref_too_many_keys' => 'Погрешна <code>&lt;ref&gt;</code> ознака;
грешни имиња, т.е. ги има многу',
	'cite_error_ref_no_input' => 'Погрешна <code>&lt;ref&gt;</code> ознака;
навод без име мора да има содржина',
	'cite_error_references_invalid_parameters' => 'Погрешна <code>&lt;references&gt;</code> ознака;
не се допуштени параметри.
Употребувајте <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Погрешна <code>&lt;references&gt;</code> ознака;
само параметарот "group" е допуштен.
Употребувајте <code>&lt;references /&gt;</code>, или <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Недостаток на беклинк ознаки.
Дефинирај повеќе во пораката <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_no_link_label_group' => 'Се потрошија називите на прилагодените врски за групата „$1“.
Определете уште во пораката <nowiki>[[MediaWiki:$2]]</nowiki>.',
	'cite_error_references_no_text' => 'Погрешна <code>&lt;ref&gt;</code> ознака;
нема текст за наводот означен како <code>$1</code>',
	'cite_error_included_ref' => 'Затворање на &lt;/ref&gt; недостасува за &lt;ref&gt; ознака',
	'cite_error_refs_without_references' => 'Постојат <code>&lt;ref&gt;</code> ознаки, но нема <code>&lt;references/&gt;</code> ознака',
	'cite_error_group_refs_without_references' => 'Постојат <code>&lt;ref&gt;</code> ознаки за група именувана како "$1", но нема соодветна <code>&lt;references group="$1"/&gt;</code> ознака',
	'cite_error_references_group_mismatch' => 'Ознакат <code>&lt;ref&gt;</code> во <code>&lt;references&gt;</code> има спротиставен групен атрибут „$1“.',
	'cite_error_references_missing_group' => 'Ознаката <code>&lt;ref&gt;</code> дефинирана во <code>&lt;references&gt;</code> има групен атрибут „$1“ кој не се јавува во претходен текст.',
	'cite_error_references_missing_key' => 'Ознаката <code>&lt;ref&gt;</code> со име „$1“ дефинирана во <code>&lt;references&gt;</code> не се користи во претходен текст.',
	'cite_error_references_no_key' => 'ознака <code>&lt;ref&gt;</code> дефинирана во <code>&lt;referencesgt;</code> нема именски атрибут.',
	'cite_error_empty_references_define' => 'Ознаката <code>&lt;ref&gt;</code> дефинирана во <code>&lt;references&gt;</code> со име „$1“ нема содржина.',
);

/** Malayalam (മലയാളം)
 * @author Praveenp
 * @author Shijualex
 */
$messages['ml'] = array(
	'cite-desc' => 'അവലംബം ചേർക്കുവാൻ ഉപയോഗിക്കാനുള്ള <nowiki><ref[ name=id]></nowiki>, <nowiki><references/></nowiki> എന്നീ ടാഗുകൾ ചേർക്കുന്നു',
	'cite_croak' => 'സൈറ്റ് ചത്തിരിക്കുന്നു; $1: $2',
	'cite_error_key_str_invalid' => 'ആന്തരിക പിഴവ്; 
അസാധുവായ $str അല്ലെങ്കിൽ $key.
ഇതു ഒരിക്കലും സംഭവിക്കാൻ പാടില്ലായിരുന്നു.',
	'cite_error_stack_invalid_input' => 'ആന്തരിക പിഴവ്; അസാധുവായ സ്റ്റാക് കീ. ഇതു ഒരിക്കലും സംഭവിക്കാൻ പാടില്ലായിരുന്നു.',
	'cite_error' => 'ഉദ്ധരിച്ചതിൽ പിഴവ്: $1',
	'cite_error_ref_numeric_key' => 'അസാധുവായ <code>&lt;ref&gt;</code> ടാഗ്;
നാമത്തിൽ സംഖ്യ മാത്രമായി അനുവദനീയമല്ല. എന്തെങ്കിലും ലഘുവിവരണം ഉപയോഗിക്കുക.',
	'cite_error_ref_no_key' => 'അസാധുവായ <code>&lt;ref&gt;</code> ടാഗ്;
ഉള്ളടക്കമൊന്നുമില്ലാത്ത അവലംബത്തിനും ഒരു പേരു വേണം.',
	'cite_error_ref_too_many_keys' => 'അസാധുവായ <code>&lt;ref&gt;</code> ടാഗ്;
അസാധുവായ പേരുകൾ, ഉദാ: too many',
	'cite_error_ref_no_input' => 'അസാധുവായ <code>&lt;ref&gt;</code> ടാഗ്;
പേരില്ലാത്ത അവലംബത്തിനു ഉള്ളടക്കമുണ്ടായിരിക്കണം.',
	'cite_error_references_invalid_parameters' => 'അസാധുവായ <code>&lt;references&gt;</code> ടാഗ്;
റെഫറൻസ് ടാഗിനകത്ത് പരാമീററ്ററുകൾ അനുവദനീയമല്ല. പകരം ഇങ്ങനെ <code>&lt;references /&gt;</code> ചെയ്യാവുന്നതാണു.',
	'cite_error_references_invalid_parameters_group' => 'അസാധുവായ <code>&lt;references&gt;</code> ടാഗ്;
റെഫറൻസ് ടാഗിനകത്ത് "group" പരാമീറ്റർ മാത്രമേ അനുവദനീമായുള്ളൂ. പകരം ഇങ്ങനെ <code>&lt;references /&gt;</code>, അല്ലെങ്കിൽ <code>&lt;references group="..." /&gt;</code> ചെയ്യാവുന്നതാണു.',
	'cite_error_references_no_backlink_label' => 'പിൻകണ്ണികൾക്കായി നൽകുന്ന ഇച്ഛാനുസരണ കുറികൾ തീർന്നുപോയിരിക്കുന്നു.
കൂടുതൽ [[MediaWiki:Cite references link many format backlink labels]] സന്ദേശത്തിൽ നിർവചിക്കുക.',
	'cite_error_no_link_label_group' => '"$1" സംഘത്തിലെ കണ്ണികൾക്കായി നൽകുന്ന ഇച്ഛാനുസരണ കുറികൾ തീർന്നുപോയിരിക്കുന്നു.
കൂടുതൽ <nowiki>[[MediaWiki:$2]]</nowiki> സന്ദേശത്തിൽ നിർവചിക്കുക.',
	'cite_error_references_no_text' => 'അസാധുവായ <code>&lt;ref&gt;</code> ടാഗ്;
<code>$1</code> എന്ന അവലംബങ്ങൾക്ക് ടെക്സ്റ്റ് ഒന്നും കൊടുത്തിട്ടില്ല.',
	'cite_error_included_ref' => '&lt;ref&gt; റ്റാഗിനു &lt;/ref&gt; എന്ന അന്ത്യറ്റാഗ് നൽകിയിട്ടില്ല',
	'cite_error_refs_without_references' => '<code>&lt;ref&gt;</code> റ്റാഗുകൾ നൽകിയിട്ടുണ്ട്, പക്ഷേ <code>&lt;references/&gt;</code> റ്റാഗ് കണ്ടെത്താനായില്ല.',
	'cite_error_group_refs_without_references' => '<code>&lt;ref&gt;</code> റ്റാഗുകൾ "$1" സംഘത്തിൽ ഉണ്ട്, പക്ഷേ ബന്ധപ്പെട്ട <code>&lt;references group="$1"/&gt;</code> റ്റാഗ് കണ്ടെത്താനായില്ല',
	'cite_error_references_group_mismatch' => '<code>&lt;ref&gt;</code> റ്റാഗിലേയും <code>&lt;references&gt;</code> എന്നതിലേയും സംഘ ഘടകമായ "$1" ഒത്തുപോകുന്നില്ല.',
	'cite_error_references_missing_group' => '<code>&lt;ref&gt;</code> റ്റാഗ് നിർവചിച്ചിട്ടുണ്ടെങ്കിലും <code>&lt;references&gt;</code> എന്നതിലുള്ള സംഘ ഘടകം "$1" ആദ്യ എഴുത്തിൽ കാണുന്നില്ല.',
	'cite_error_references_missing_key' => '<code>&lt;ref&gt;</code> റ്റാഗ് "$1" എന്ന പേരോടെ <code>&lt;references&gt;</code> എന്നതിൽ നിർവചിച്ചിട്ടുണ്ടെങ്കിലും ആദ്യ എഴുത്തിൽ ഉപയോഗിക്കുന്നില്ല.',
	'cite_error_references_no_key' => '<code>&lt;ref&gt;</code> റ്റാഗ്  <code>&lt;references&gt;</code> എന്നതിൽ നിർവചിച്ചിട്ടുണ്ടെങ്കിലും നാമ ഘടകം നൽകിയിട്ടില്ല.',
	'cite_error_empty_references_define' => ' <code>&lt;references&gt;</code> ആവശ്യത്തിനായി "$1" എന്ന പേരിൽ നിർ‌വചിക്കപ്പെട്ട <code>&lt;ref&gt;</code> റ്റാഗിന് ഉള്ളടക്കമൊന്നുമില്ല.',
);

/** Marathi (मराठी)
 * @author Kaustubh
 */
$messages['mr'] = array(
	'cite-desc' => '<nowiki><ref[ name=id]></nowiki> व <nowiki><references/></nowiki> हे दोन संदर्भ देण्यासाठी वापरण्यात येणारे शब्द वाढविले जातील.',
	'cite_croak' => 'संदर्भ देता आला नाही; $1: $2',
	'cite_error_key_str_invalid' => 'अंतर्गत त्रुटी; चुकीचे $str आणि/किंवा $key. असे कधीही घडले नाही पाहिजे.',
	'cite_error_stack_invalid_input' => 'अंतर्गत त्रुटी; चुकीची स्टॅक चावी. असे कधीही घडले नाही पाहिजे.',
	'cite_error' => 'त्रूटी उधृत करा: $1',
	'cite_error_ref_numeric_key' => '<code>&lt;ref&gt;</code> चुकीचा कोड; नाव हे पूर्णांकी संख्या असू शकत नाही, कृपया माहितीपूर्ण शीर्षक द्या',
	'cite_error_ref_no_key' => '<code>&lt;ref&gt;</code> चुकीचा कोड; रिकाम्या संदर्भांना नाव असणे गरजेचे आहे',
	'cite_error_ref_too_many_keys' => '<code>&lt;ref&gt;</code> चुकीचा कोड; चुकीची नावे, उदा. खूप सारी',
	'cite_error_ref_no_input' => '<code>&lt;ref&gt;</code> चुकीचा कोड; निनावी संदर्भांमध्ये माहिती असणे गरजेचे आहे',
	'cite_error_references_invalid_parameters' => '<code>&lt;references&gt;</code> चुकीचा कोड; पॅरॅमीटर्स देता येत नाही, <code>&lt;references /&gt;</code> हा कोड वापरा',
	'cite_error_references_invalid_parameters_group' => 'चुकीची <code>&lt;references&gt;</code> खूण; फक्त पॅरॅमीटर चा गट वापरता येईल, <code>&lt;references /&gt;</code> किंवा <code>&lt;references group="..." /&gt;</code> चा वापर करा',
	'cite_error_references_no_backlink_label' => 'तयार केलेली मागीलदुवे देणारी नावे संपलेली आहेत, अधिक नावे <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki> या प्रणाली संदेशात लिहा',
	'cite_error_references_no_text' => '<code>&lt;ref&gt;</code> चुकीचा कोड; <code>$1</code> नावाने दिलेल्या संदर्भांमध्ये काहीही माहिती नाही',
);

/** Malay (Bahasa Melayu)
 * @author Aviator
 * @author Izzudin
 */
$messages['ms'] = array(
	'cite-desc' => 'Menambah tag <nowiki><ref[ name=id]></nowiki> dan <nowiki><references/></nowiki> untuk pemetikan',
	'cite_croak' => 'Ralat maut petik; $1: $2',
	'cite_error_key_str_invalid' => 'Ralat dalaman; str dan/atau $key tidak sah.',
	'cite_error_stack_invalid_input' => 'Ralat dalaman; kunci tindanan tidak sah.',
	'cite_error' => 'Ralat petik: $1',
	'cite_error_ref_numeric_key' => 'Tag <code>&lt;ref&gt;</code> tidak sah; nombor ringkas tidak dibenarkan, sila masukkan tajuk yang lebih terperinci',
	'cite_error_ref_no_key' => 'Tag <code>&lt;ref&gt;</code> tidak sah; rujukan tanpa kandungan mestilah mempunyai nama',
	'cite_error_ref_too_many_keys' => 'Tag <code>&lt;ref&gt;</code> tidak sah; nama-nama tidak sah, misalnya terlalu banyak',
	'cite_error_ref_no_input' => "'Tag <code>&lt;ref&gt;</code> tidak sah; rujukan tanpa nama mestilah mempunyai kandungan",
	'cite_error_references_invalid_parameters' => 'Tag <code>&lt;references&gt;</code> tidak sah; parameter tidak dibenarkan, gunakan <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Tag <code>&lt;references&gt;</code> tidak sah; hanya parameter "group" dibenarkan.
Gunakan <code>&lt;references /&gt;</code> atau <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Kehabisan label pautan balik tempahan. Sila tambah label dalam pesanan <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_references_no_text' => 'Tag <code>&lt;ref&gt;</code> tidak sah; teks bagi rujukan <code>$1</code> tidak disediakan',
	'cite_error_included_ref' => 'Tag &lt;ref&gt; tidak ditutup dengan &lt;/ref&gt;',
	'cite_error_refs_without_references' => 'Tag <code>&lt;ref&gt;</code> ada tetapi tag <code>&lt;references/&gt;</code> tidak disertakan',
	'cite_error_group_refs_without_references' => 'Tag <code>&lt;ref&gt;</code> untuk kumpulan "$1" ada tetapi tag <code>&lt;references group="$1"/&gt;</code> yang sepadan tidak disertakan',
);

/** Low German (Plattdüütsch)
 * @author Slomox
 */
$messages['nds'] = array(
	'cite-desc' => 'Föögt <nowiki><ref[ name=id]></nowiki> un <nowiki><references/></nowiki> Tags för Zitaten to',
	'cite_croak' => 'Fehler bi de Referenzen. $1: $2',
	'cite_error_key_str_invalid' => 'Internen Fehler: ungülligen $str un/oder $key. Dat schull egentlich nie vörkamen.',
	'cite_error_stack_invalid_input' => 'Internen Fehler: ungülligen Stack-Slötel. Dat schull egentlich nie vörkamen.',
	'cite_error' => 'Zitat-Fehler: $1',
	'cite_error_ref_numeric_key' => 'Ungülligen Tag <tt>&lt;ref&gt;</tt>: de Naam dröff keen reine Tall wesen, bruuk en Naam, de de Saak beschrifft.',
	'cite_error_ref_no_key' => 'Ungülligen Tag <tt>&lt;ref&gt;</tt>: „ref“ ahn Inholt mutt en Naam hebben.',
	'cite_error_ref_too_many_keys' => 'Ungülligen Tag <tt>&lt;ref&gt;</tt>: ungüllige Naams, to’n Bispeel to veel.',
	'cite_error_ref_no_input' => 'Ungülligen Tag <tt>&lt;ref&gt;</tt>: „ref“ ahn Naam mutt en Inholt hebben.',
	'cite_error_references_invalid_parameters' => 'Ungülligen Tag <code>&lt;references&gt;</code>: Parameters sünd nich verlöövt, bruuk <tt>&lt;references /&gt;</tt>',
	'cite_error_references_invalid_parameters_group' => 'Ungülligen Tag <code>&lt;references&gt;</code>: Blot de Parameter „group“ is verlöövt, bruuk <tt>&lt;references /&gt;</tt> oder <tt>&lt;references group="..." /&gt;</tt>',
	'cite_error_references_no_backlink_label' => 'De verföögboren Tekens för de Lenken op Referenzen sünd all. Dat lett sik repareren, wenn in de Systemnaricht <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki> mehr Tekens angeven warrt.',
	'cite_error_references_no_text' => 'Ungülligen Tag <tt>&lt;ref&gt;</tt>; is keen Text för Refs mit den Naam <tt>$1</tt> angeven.',
	'cite_error_included_ref' => 'Dor fehlt en tosluten &lt;/ref&gt;',
	'cite_error_refs_without_references' => '<code>&lt;ref&gt;</code>-Tags gifft dat, is aver keen <code>&lt;references/&gt;</code>-Tag funnen worrn.',
	'cite_error_group_refs_without_references' => '<code>&lt;ref&gt;</code>-Tags för de Grupp „$1“ gifft dat, is aver keen <code>&lt;references group=„$1“/&gt;</code>-Tag funnen worrn',
);

/** Nedersaksisch (Nedersaksisch)
 * @author Servien
 */
$messages['nds-nl'] = array(
	'cite_croak' => "Fout in 't referentiesysteem; $1: $2",
	'cite_error' => 'Citeerfout: $1',
);

/** Dutch (Nederlands)
 * @author Niels
 * @author Siebrand
 */
$messages['nl'] = array(
	'cite-desc' => 'Voegt <nowiki><ref[ name=id]></nowiki> en <nowiki><references/></nowiki> tags toe voor citaten',
	'cite_croak' => 'Probleem met Cite; $1: $2',
	'cite_error_key_str_invalid' => 'Interne fout;
onjuiste $str and/of $key.
Dit zou niet voor moeten komen.',
	'cite_error_stack_invalid_input' => 'Interne fout;
onjuiste stacksleutel.
Dit zou niet voor moeten komen.',
	'cite_error' => 'Citefout: $1',
	'cite_error_ref_numeric_key' => 'Onjuiste tag <code>&lt;ref&gt;</code>;
de naam kan geen eenvoudige integer zijn.
Gebruik een beschrijvende titel',
	'cite_error_ref_no_key' => 'Onjuiste tag <code>&lt;ref&gt;</code>;
refs zonder inhoud moeten een naam hebben',
	'cite_error_ref_too_many_keys' => 'Onjuiste tag <code>&lt;ref&gt;</code>;
onjuiste namen, bijvoorbeeld te veel',
	'cite_error_ref_no_input' => 'Onjuiste tag <code>&lt;ref&gt;</code>;
refs zonder naam moeten inhoud hebben',
	'cite_error_references_invalid_parameters' => 'Onjuiste tag <code>&lt;references&gt;</code>;
parameters zijn niet toegestaan.
Gebruik <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Onjuiste tag <code>&lt;references&gt;</code>;
alleen de parameter "group" is toegestaan.
Gebruik <code>&lt;references /&gt;</code>, of <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Het aantal beschikbare backlinklabels is opgebruikt.
Geef meer labels op in het bericht <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_no_link_label_group' => 'Het aantal aangepaste verwijzinglabels voor de group "$1" is uitgeput.
U kunt er meer instellen in het systeembericht <nowiki>[[MediaWiki:$2]]</nowiki>.',
	'cite_error_references_no_text' => 'Onjuiste tag <code>&lt;ref&gt;</code>;
er is geen tekst opgegeven voor refs met de naam <code>$1</code>',
	'cite_error_included_ref' => 'Na het label &lt;ref&gt; ontbreekt het afsluitende label &lt;/ref&gt;',
	'cite_error_refs_without_references' => 'De tag <code>&lt;ref&gt;</code> bestaat, maar de tag <code>&lt;references/&gt;</code> is niet aangetroffen',
	'cite_error_group_refs_without_references' => 'Er bestaat een tag <code>&lt;ref&gt;</code> voor de groep "$1", maar er is geen bijbehorende tag <code>&lt;references group="$1"/&gt;</code> aangetroffen',
	'cite_error_references_group_mismatch' => 'De tag <code>&lt;ref&gt;</code> in <code>&lt;references&gt;</code> conflicteert met groepseigenschap "$1".',
	'cite_error_references_missing_group' => 'De tag <code>&lt;ref&gt;</code> die is gedefinieerd in <code>&lt;references&gt;</code> heeft de groepseigenschap "$1" niet niet eerder in te tekst voorkomt.',
	'cite_error_references_missing_key' => 'De tag <code>&lt;ref&gt;</code> met de naam "$1" gedefiniteerd in <code>&lt;references&gt;</code> wordt niet eerder in de tekst gebruikt.',
	'cite_error_references_no_key' => 'De tag <code>&lt;ref&gt;</code> die is gedefinieerd in <code>&lt;references&gt;</code> heeft geen eigenschapsnaam.',
	'cite_error_empty_references_define' => 'De tag <code>&lt;ref&gt;</code> die is gedefinieerd in <code>&lt;references&gt;</code> met de naam "$1" heeft geen inhoud.',
);

/** Norwegian Nynorsk (‪Norsk (nynorsk)‬)
 * @author Frokor
 * @author Harald Khan
 */
$messages['nn'] = array(
	'cite-desc' => 'Legg til <nowiki><ref[ name=id]></nowiki> og <nowiki><references/></nowiki>-merke for referansar',
	'cite_croak' => 'Feil i fotnotesystemet; $1: $2',
	'cite_error_key_str_invalid' => 'Intern feil: Ugyldig $str og/eller $key. Dette burde aldri skjedd.',
	'cite_error_stack_invalid_input' => 'Intern feil; ugyldig stakknøkkel. Dette burde aldri skjedd.',
	'cite_error' => 'Referansefeil: $1',
	'cite_error_ref_numeric_key' => 'Ugyldig <code>&lt;ref&gt;</code>-kode; namnet kan ikkje vere eit enkelt heiltal, bruk ein skildrande tittel',
	'cite_error_ref_no_key' => 'Ugyldig <code>&lt;ref&gt;</code>-kode; referansar utan innhald må innehalde namn',
	'cite_error_ref_too_many_keys' => 'Ugyldig <code>&lt;ref&gt;</code>-kode; ugyldige namn, t.d. for mange',
	'cite_error_ref_no_input' => 'Ugyldig <code>&lt;ref&gt;</code>-kode; referansar uten namn må ha innhald',
	'cite_error_references_invalid_parameters' => 'Ugyldig <code>&lt;references&gt;</code>-kode; ingen parametrar er tillat, bruk <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Ugyldig <code>&lt;references&gt;</code>-tagg; berre parameteren «group» er tillatt. Bruk <code>&lt;references /&gt;</code> eller <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Gjekk tom for eigendefinerte tilbakelenketekstar.
Definer fleire i meldinga <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_references_no_text' => 'Ugyldig <code>&lt;ref&gt;</code>-tagg; ingen tekst vart gjeve for referansen med namnet <code>$1</code>',
	'cite_error_included_ref' => 'Avsluttande &lt;/ref&gt;-tagg manglar for &lt;ref&gt;',
	'cite_error_refs_without_references' => '<code>&lt;ref&gt;</code>-merke finst, men eit <code>&lt;references/&gt;</code>-merke finst ikkje',
	'cite_error_group_refs_without_references' => '<code>&lt;ref&gt;</code>-merke finst for gruppenamnet «$1», men inkje samsvarande <code>&lt;references group="$1"/&gt;</code>-merke vart funne',
);

/** Norwegian (bokmål)‬ (‪Norsk (bokmål)‬)
 * @author Audun
 * @author Jon Harald Søby
 * @author Laaknor
 */
$messages['no'] = array(
	'cite-desc' => 'Legger til <nowiki><ref[ name=id]></nowiki> og <nowiki><references/></nowiki>-tagger for referanser',
	'cite_croak' => 'Sitering døde; $1: $2',
	'cite_error_key_str_invalid' => 'Intern feil: Ugyldig $str og/eller $key. Dette burde aldri forekomme.',
	'cite_error_stack_invalid_input' => 'Intern feil; ugyldig stakknøkkel. Dette burde aldri forekomme.',
	'cite_error' => 'Siteringsfeil: $1',
	'cite_error_ref_numeric_key' => 'Ugyldig <code>&lt;ref&gt;</code>-kode; navnet kan ikke være et enkelt heltall, bruk en beskrivende tittel',
	'cite_error_ref_no_key' => 'Ugyldig <code>&lt;ref&gt;</code>-kode; referanser uten innhold må inneholde navn',
	'cite_error_ref_too_many_keys' => 'Ugyldig <code>&lt;ref&gt;</code>-kode; ugyldige navn, f.eks. for mange',
	'cite_error_ref_no_input' => 'Ugyldig <code>&lt;ref&gt;</code>-kode; referanser uten navn må ha innhold',
	'cite_error_references_invalid_parameters' => 'Ugyldig <code>&lt;references&gt;</code>-kode; ingen parametere tillates, bruk <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Ugyldig <code>&lt;references&gt;</code>-tagg; kun parameteret «group» tillates. Bruk <code>&lt;references /&gt;</code> eller <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Gikk tom for egendefinerte tilbakelenketekster.
Definer flere i beskjeden <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>.',
	'cite_error_no_link_label_group' => 'Gikk tom for egendefinerte lenkemerker for gruppen «$1».
Definér fler i <nowiki>[[MediaWiki:$2]]</nowiki>-beskjeden.',
	'cite_error_references_no_text' => 'Ugyldig <code>&lt;ref&gt;</code>-tagg; ingen tekst ble oppgitt for referansen ved navn <code>$1</code>',
	'cite_error_included_ref' => 'Avsluttende &lt;/ref&gt;-tagg mangler for &lt;ref&gt;',
	'cite_error_refs_without_references' => '<code>&lt;ref&gt;</code>-merker finnes, men ingen <code>&lt;references/&gt;</code>-merke funnet',
	'cite_error_group_refs_without_references' => '<code>&lt;ref&gt;</code>-merke finnes for gruppenavnet «$1», men ingen <code>&lt;references group="$1"/&gt;</code>-merking ble funnet',
	'cite_error_references_group_mismatch' => '<code>&lt;ref&gt;</code>-tagg i <code>&lt;references&gt;</code> har motstridig attributt «$1».',
	'cite_error_references_missing_group' => '<code>&lt;ref&gt;</code>-tagg definert i <code>&lt;references&gt;</code> har gruppeattributtet «$1» som ikke forekommer i teksten.',
	'cite_error_references_missing_key' => '<code>&lt;ref&gt;</code>-taggen med navnet «$1» definert i <code>&lt;references&gt;</code> brukes ikke i teksten.',
	'cite_error_references_no_key' => '<code>&lt;ref&gt;</code>-tagg definert i <code>&lt;references&gt;</code> har ikke noe navneattributt.',
	'cite_error_empty_references_define' => '<code>&lt;ref&gt;</code>-taggen i <code>&lt;references&gt;</code> med navnet «$1» har ikke noe innhold.',
	'cite_references_link_many_format_backlink_labels' => 'a b c d e f g h i j k l m n o p q r s t u v w x y z æ ø å aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay az aæ aø aå ba bb bc bd be bf bg bh bi bj bk bl bm bn bo bp bq br bs bt bu bv bw bx by bz bæ bø bå ca cb cc cd ce cf cg ch ci cj ck cl cm cn co cp cq cr cs ct cu cv cw cx cy cz cæ cø cå da db dc dd de df dg dh di dj dk dl dm dn do dp dq dr ds dt du dv dw dx dy dz dæ dø då ea eb ec ed ee ef eg eh ei ej ek el em en eo ep eq er es et eu ev ew ex ey ez eæ eø eå fa fb fc fd fe ff fg fh fi fj fk fl fm fn fo fp fq fr fs ft fu fv fw fx fy fz fæ fø få ga gb gc gd ge gf gg gh gi gj gk gl gm gn go gp gq gr gs gt gu gv gw gx gy gz gæ gø gå ha hb hc hd he hf hg hh hi hj hk hl hm hn ho hp hq hr hs ht hu hv hw hx hy hz hæ hø hå ia ib ic id ie if ig ih ii ij ik il im in io ip iq ir is it iu iv iw ix iy iz iæ iø iå ja jb jc jd je jf jg jh ji jj jk jl jm jn jo jp jq jr js jt ju jv jw jx jy jz jæ jø jå ka kb kc kd ke kf kg kh ki kj kk kl km kn ko kp kq kr ks kt ku kv kw kx ky kz kæ kø kå la lb lc ld le lf lg lh li lj lk ll lm ln lo lp lq lr ls lt lu lv lw lx ly lz læ lø lå ma mb mc md me mf mg mh mi mj mk ml mm mn mo mp mq mr ms mt mu mv mw mx my mz mæ mø må na nb nc nd ne nf ng nh ni nj nk nl nm nn no np nq nr ns nt nu nv nw nx ny nz næ nø nå oa ob oc od oe of og oh oi oj ok ol om on oo op oq or os ot ou ov ow ox oy oz oæ oø oå pa pb pc pd pe pf pg ph pi pj pk pl pm pn po pp pq pr ps pt pu pv pw px py pz pæ pø på qa qb qc qd qe qf qg qh qi qj qk ql qm qn qo qp qq qr qs qt qu qv qw qx qy qz qæ qø qå ra rb rc rd re rf rg rh ri rj rk rl rm rn ro rp rq rr rs rt ru rv rw rx ry rz ræ rø rå sa sb sc sd se sf sg sh si sj sk sl sm sn so sp sq sr ss st su sv sw sx sy sz sæ sø så ta tb tc td te tf tg th ti tj tk tl tm tn to tp tq tr ts tt tu tv tw tx ty tz tæ tø tå ua ub uc ud ue uf ug uh ui uj uk ul um un uo up uq ur us ut uu uv uw ux uy uz uæ uø uå va vb vc vd ve vf vg vh vi vj vk vl vm vn vo vp vq vr vs vt vu vv vw vx vy vz væ vø vå wa wb wc wd we wf wg wh wi wj wk wl wm wn wo wp wq wr ws wt wu wv ww wx wy wz wæ wø wå xa xb xc xd xe xf xg xh xi xj xk xl xm xn xo xp xq xr xs xt xu xv xw xx xy xz xæ xø xå ya yb yc yd ye yf yg yh yi yj yk yl ym yn yo yp yq yr ys yt yu yv yw yx yy yz yæ yø yå za zb zc zd ze zf zg zh zi zj zk zl zm zn zo zp zq zr zs zt zu zv zw zx zy zz zæ zø zå æa æb æc æd æe æf æg æh æi æj æk æl æm æn æo æp æq ær æs æt æu æv æw æx æy æz ææ æø æå øa øb øc ød øe øf øg øh øi øj øk øl øm øn øo øp øq ør øs øt øu øv øw øx øy øz øæ øø øå åa åb åc åd åe åf åg åh åi åj åk ål åm ån åo åp åq år ås åt åu åv åw åx åy åz åæ åø åå',
);

/** Occitan (Occitan)
 * @author Cedric31
 */
$messages['oc'] = array(
	'cite-desc' => 'Apond las balisas <nowiki><ref[ name=id]></nowiki> e <nowiki><references/></nowiki>, per las citacions.',
	'cite_croak' => 'Citacion corrompuda ; $1 : $2',
	'cite_error_key_str_invalid' => 'Error intèrna ; 
$str o $key incorrèctes.
Aquò se deuriá pas jamai produsir.',
	'cite_error_stack_invalid_input' => 'Error intèrna ; clau de pila invalida',
	'cite_error' => 'Error de citacion : $1',
	'cite_error_ref_numeric_key' => 'Ampèl invalid ; clau non-integrala esperada',
	'cite_error_ref_no_key' => 'Ampèl invalid ; cap de clau pas especificada',
	'cite_error_ref_too_many_keys' => 'Ampèl invalid ; claus invalidas, per exemple, tròp de claus especificadas o clau erronèa',
	'cite_error_ref_no_input' => 'Ampèl invalid ; cap de dintrada pas especificada',
	'cite_error_references_invalid_parameters' => 'Arguments invalids ; argument esperat',
	'cite_error_references_invalid_parameters_group' => 'Balisa <code>&lt;references&gt;</code> incorrècta ;

sol lo paramètre « group » es autorizat.

Utilizatz <code>&lt;references /&gt;</code>, o alara <code>&lt;references group="..." /&gt;</code>.',
	'cite_error_references_no_backlink_label' => 'Execucion en defòra de las etiquetas personalizadas, definissetz mai dins lo messatge <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_references_no_text' => 'Balisa  <code>&lt;ref&gt;</code> incorrècta ;

pas de tèxte per las referéncias nomenadas <code>$1</code>.',
	'cite_error_included_ref' => 'Clausura &lt;/ref&gt; omesa per la balisa &lt;ref&gt;',
	'cite_error_refs_without_references' => 'La balisa <code>&lt;ref&gt;</code> existís, mas cap de balisa <code>&lt;references/&gt;</code> pas trobada.',
	'cite_error_group_refs_without_references' => 'La balisa <code>&lt;ref&gt;</code> existís per un grop nomenat « $1 », mas cap de balisa <code>&lt;references group="$1"/&gt;</code> correspondenta pas trobada',
	'cite_error_references_group_mismatch' => "La balisa <code>&lt;ref&gt;</code> dins <code>&lt;references&gt;</code> a l'atribut de grop « $1 » que dintra en conflicte amb lo de <code>&lt;references&gt;</code>.",
	'cite_error_references_missing_group' => 'La balisa <code>&lt;ref&gt;</code> definida dins <code>&lt;references&gt;</code> a un gropat atribuit « $1 » que figura pas dins lo tèxte precedent.',
	'cite_error_references_missing_key' => 'La balisa <code>&lt;ref&gt;</code> amb lo nom « $1 » definida dins <code>&lt;references&gt;</code> es pas utilizada dins lo tèxte precedent.',
	'cite_error_references_no_key' => 'La balisa <code>&lt;ref&gt;</code> definida dins <code>&lt;references&gt;</code> a pas de nom d’atribut.',
	'cite_error_empty_references_define' => 'La balisa <code>&lt;ref&gt;</code> definida dins <code>&lt;references&gt;</code> amb lo nom « $1 » a pas de contengut.',
);

/** Pangasinan (Pangasinan) */
$messages['pag'] = array(
	'cite_error' => 'Bitlaen so error $1; $2',
);

/** Polish (Polski)
 * @author Derbeth
 * @author Holek
 * @author Leinad
 * @author Sp5uhe
 */
$messages['pl'] = array(
	'cite-desc' => 'Dodaje znaczniki <nowiki><ref[ name=id]></nowiki> i <nowiki><references/></nowiki> ułatwiające podawanie źródeł cytatów',
	'cite_croak' => 'Cytowanie nieudane; $1: $2',
	'cite_error_key_str_invalid' => 'Błąd wewnętrzny;
nieprawidłowy $str i/lub $key.
To nigdy nie powinno się zdarzyć.',
	'cite_error_stack_invalid_input' => 'Błąd wewnętrzny – nieprawidłowy klucz sterty. To nigdy nie powinno się zdarzyć.',
	'cite_error' => "Błąd rozszerzenia ''cite'': $1",
	'cite_error_ref_numeric_key' => 'Nieprawidłowy znacznik <code>&lt;ref&gt;</code>. Nazwa nie może być liczbą, użyj nazwy opisowej.',
	'cite_error_ref_no_key' => 'Nieprawidłowy znacznik <code>&lt;ref&gt;</code>. Odnośnik ref z zawartością musi mieć nazwę.',
	'cite_error_ref_too_many_keys' => 'Nieprawidłowe nazwy parametrów elementu <code>&lt;ref&gt;</code>.',
	'cite_error_ref_no_input' => 'Błąd w składni elementu <code>&lt;ref&gt;</code>. Przypisy bez podanej nazwy muszą posiadać treść',
	'cite_error_references_invalid_parameters' => 'Błąd w składni elementu <code>&lt;references&gt;</code>. Nie można wprowadzać parametrów do tego elementu, użyj <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Nieprawidłowy znacznik <code>&lt;references&gt;</code>;
dostępny jest wyłącznie parametr „group”.
Użyj znacznika <code>&lt;references /&gt;</code>, lub <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Zabrakło etykiet do przypisów.
Zadeklaruj więcej w komunikacie <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_no_link_label_group' => 'Zabrakło niestandardowych etykiet linków dla grupy „$1“.
Zdefiniuj ich większą liczbę w komunikacie <nowiki>[[MediaWiki:$2]]</nowiki>.',
	'cite_error_references_no_text' => 'Błąd w składni elementu <code>&lt;ref&gt;</code>. Brak tekstu w przypisie o nazwie <code>$1</code>',
	'cite_error_included_ref' => 'Brak znacznika zamykającego &lt;/ref&gt; po otwartym znaczniku &lt;ref&gt;',
	'cite_error_refs_without_references' => 'Istnieje znacznik <code>&lt;ref&gt;</code>, ale nie odnaleziono znacznika <code>&lt;references/&gt;</code>',
	'cite_error_group_refs_without_references' => 'Istnieje znacznik <code>&lt;ref&gt;</code> dla grupy o nazwie „$1”, ale nie odnaleziono odpowiedniego znacznika <code>&lt;references group="$1"/&gt;</code>',
	'cite_error_references_group_mismatch' => 'Znacznik <code>&lt;ref&gt;</code> w <code>&lt;references&gt;</code> nie może mieć atrybutu grupy „$1”.',
	'cite_error_references_missing_group' => 'Znacznik <code>&lt;ref&gt;</code> zdefiniowany w <code>&lt;references&gt;</code> ma atrybut grupowania „$1”, który nie występuje wcześniej w treści.',
	'cite_error_references_missing_key' => 'Znacznik <code>&lt;ref&gt;</code> o nazwie „$1”, zdefiniowany w <code>&lt;references&gt;</code>, nie był użyty wcześniej w treści.',
	'cite_error_references_no_key' => 'Znacznik <code>&lt;ref&gt;</code> zdefiniowany w <code>&lt;references&gt;</code> nie ma atrybutu <code>name</code>.',
	'cite_error_empty_references_define' => 'Znacznik <code>&lt;ref&gt;</code> zdefiniowany w <code>&lt;references&gt;</code> o nazwie „$1” nie ma treści.',
);

/** Piedmontese (Piemontèis)
 * @author Borichèt
 * @author Dragonòt
 */
$messages['pms'] = array(
	'cite-desc' => 'A gionta le tichëtte <nowiki><ref[ name=id]></nowiki> e <nowiki><references/></nowiki>, për sitassion',
	'cite_croak' => 'Sitassion mòrta; $1: $2',
	'cite_error_key_str_invalid' => 'Eror antern;
$str e/o $key sbalià.
Sòn a dovrìa mai capité.',
	'cite_error_stack_invalid_input' => "Eror antern;
ciav d'ambaronament pa bon-a.
Sòn a dovrìa mai capité.",
	'cite_error' => 'Eror ëd sitassion: $1',
	'cite_error_ref_numeric_key' => "Tichëtta <code>&lt;ref&gt;</code> pa bon-a;
ël nòm a peul pa esse n'antregh sempi. Deuvra un tìtol descritiv.",
	'cite_error_ref_no_key' => "Tichëtta <code>&lt;ref&gt;</code> pa bon-a;
j'arferiment sensa contnù a devo avèj un nòm",
	'cite_error_ref_too_many_keys' => 'Tichëtta <code>&lt;ref&gt;</code> pa bon-a;
nòm pa bon, për esempi tròpi',
	'cite_error_ref_no_input' => "Tichëtta <code>&lt;ref&gt;</code> pa bon-a;
j'arferiment sensa nòm a devo avèj un contnù",
	'cite_error_references_invalid_parameters' => "Tichëtta <code>&lt;references&gt;</code> pa bon-a;
pa gnun paràmetr përmëttù.
Ch'a deuvra <code>&lt;references /&gt;</code>",
	'cite_error_references_invalid_parameters_group' => 'Tichëtta <code>&lt;references&gt;</code> pa bon-a;
as peul mach dovresse ël paràmetr "group".
Ch\'a deuvra <code>&lt;references /&gt;</code>, o <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Etichëtte ëd backlink përsonalisà esaurìe.
Definiss-ne ëd pì ant ël messagi <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_no_link_label_group' => 'Surtì fòra dle tichëtte dij colegament utent për la partìa "$1".
Definissne ëd pi ant ël mëssagi <nowiki>[[MediaWiki:$2]]</nowiki>.',
	'cite_error_references_no_text' => "Tichëtta <code>&lt;ref&gt;</code> pa bon-a;
pa gnun test a l'é stàit dàit për l'arferiment ciamà <code>$1</code>",
	'cite_error_included_ref' => '&lt;/ref&gt; sarà mancant për la tichëtta &lt;ref&gt;',
	'cite_error_refs_without_references' => "la tichëtta <code>&lt;ref&gt;</code> a esist, ma gnun-a tichëtta <code>&lt;references/&gt;</code> a l'é stàita trovà",
	'cite_error_group_refs_without_references' => 'Dle tichëtte <code>&lt;ref&gt;</code> a esisto për na partìa ciamà "$1", ma gnun-a tichëtta corëspondenta <code>&lt;references group="$1"/&gt;</code> a l\'é stàita trovà',
	'cite_error_references_group_mismatch' => 'La tichëtta <code>&lt;ref&gt;</code> an <code>&lt;references&gt;</code> a l\'ha n\'atribut ëd partìa "$1" an conflit.',
	'cite_error_references_missing_group' => 'La tichëtta <code>&lt;ref&gt;</code> definìa an <code>&lt;references&gt;</code> a l\'ha n\'atribut ëd partìa "$1" che a l\'era pa ant ël test prima.',
	'cite_error_references_missing_key' => 'La tichëtta <code>&lt;ref&gt;</code> con nòm "$1" definìa an <code>&lt;references&gt;</code> a l\'é pa dovrà ant ël test prima.',
	'cite_error_references_no_key' => "La tichëtta <code>&lt;ref&gt;</code> definìa an <code>&lt;references&gt;</code> a l'ha pa gnun atribut ëd nòm.",
	'cite_error_empty_references_define' => 'La tichëtta <code>&lt;ref&gt;</code> definìa an <code>&lt;references&gt;</code> con nòm "$1" a l\'ha pa gnun contnù.',
);

/** Portuguese (Português)
 * @author 555
 * @author Crazymadlover
 * @author Giro720
 * @author Hamilton Abreu
 * @author Malafaya
 * @author Waldir
 */
$messages['pt'] = array(
	'cite-desc' => 'Adiciona elementos <nowiki><ref[ name=id]></nowiki> e <nowiki><references/></nowiki> para uso em citações',
	'cite_croak' => 'Citação com problemas; $1: $2',
	'cite_error_key_str_invalid' => 'Erro interno;
$str e/ou $key inválido.
Isto nunca deveria acontecer.',
	'cite_error_stack_invalid_input' => 'Erro interno; chave fixa inválida',
	'cite_error' => 'Erro de citação $1',
	'cite_error_ref_numeric_key' => 'Código <code>&lt;ref&gt;</code> inválido; o nome não pode ser um número. Utilize um nome descritivo',
	'cite_error_ref_no_key' => 'Código <code>&lt;ref&gt;</code> inválido; refs sem conteúdo devem ter um parâmetro de nome',
	'cite_error_ref_too_many_keys' => 'Código <code>&lt;ref&gt;</code> inválido; nomes inválidos (por exemplo, nome muito extenso)',
	'cite_error_ref_no_input' => 'Código <code>&lt;ref&gt;</code> inválido; refs sem parâmetro de nome devem possuir conteúdo a elas associado',
	'cite_error_references_invalid_parameters' => 'Código <code>&lt;references&gt;</code> inválido; não são permitidos parâmetros. Utilize como <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'O elemento <code>&lt;references&gt;</code> é inválido;
só é permitido o parâmetro "group".
Use <code>&lt;references /&gt;</code>, ou <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Esgotamento das legendas personalizadas para backlinks.
Defina mais na mensagem <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_no_link_label_group' => 'Esgotamento das legendas personalizadas para links, no grupo "$1".
Defina mais na mensagem <nowiki>[[MediaWiki:$2]]</nowiki>.',
	'cite_error_references_no_text' => 'Tag <code>&lt;ref&gt;</code> inválida; não foi fornecido texto para as refs chamadas <code>$1</code>',
	'cite_error_included_ref' => '&lt;/ref&gt; de fecho em falta, para o elemento &lt;ref&gt;',
	'cite_error_refs_without_references' => 'existem tags <code>&lt;ref&gt;</code>, mas nenhuma tag <code>&lt;references/&gt;</code> foi encontrada',
	'cite_error_group_refs_without_references' => 'existem tags <code>&lt;ref&gt;</code> para um grupo chamado "$1", mas nenhuma tag <code>&lt;references group="$1"/&gt;</code> correspondente foi encontrada',
	'cite_error_references_group_mismatch' => 'A etiqueta <code>&lt;ref&gt;</code> em <code>&lt;references&gt;</code> possui atributo de grupo "$1" conflitante.',
	'cite_error_references_missing_group' => 'A etiqueta <code>&lt;ref&gt;</code> definida em <code>&lt;references&gt;</code> possui atributo de grupo "$1" que não aparece no texto acima.',
	'cite_error_references_missing_key' => 'A etiqueta <code>&lt;ref&gt;</code> com nome "$1" definida em <code>&lt;references&gt;</code> não é utilizada no texto acima.',
	'cite_error_references_no_key' => 'A etiqueta <code>&lt;ref&gt;</code> definida em <code>&lt;references&gt;</code> não possui atributo de nome.',
	'cite_error_empty_references_define' => '<code>&lt;ref&gt;</code> etiqueta definida em <code>&lt;references&gt;</code> com nome "$1" não possui conteúdo.',
);

/** Brazilian Portuguese (Português do Brasil)
 * @author Eduardo.mps
 * @author Giro720
 */
$messages['pt-br'] = array(
	'cite-desc' => 'Adiciona marcas <nowiki><ref[ name=id]></nowiki> e <nowiki><references/></nowiki> para citações',
	'cite_croak' => 'Citação com problemas; $1: $2',
	'cite_error_key_str_invalid' => 'Erro interno;
$str e/ou $key inválido.
Isto nunca deveria acontecer.',
	'cite_error_stack_invalid_input' => 'Erro interno;
chave fixa inválida.
Isto nunca deveria ocorrer.',
	'cite_error' => 'Erro de citação: $1',
	'cite_error_ref_numeric_key' => 'Marca <code>&lt;ref&gt;</code> inválida; 
o nome não pode ser um número. Utilize um título descritivo',
	'cite_error_ref_no_key' => 'Marca <code>&lt;ref&gt;</code> inválida; 
refs sem conteúdo devem ter um nome',
	'cite_error_ref_too_many_keys' => 'Marca <code>&lt;ref&gt;</code> inválida; 
nomes inválidos (por exemplo, nome muito extenso)',
	'cite_error_ref_no_input' => 'Marca <code>&lt;ref&gt;</code> inválida; 
refs sem nome devem possuir conteúdo',
	'cite_error_references_invalid_parameters' => 'Marca <code>&lt;references&gt;</code> inválida; 
não são permitidos parâmetros.
Utilize <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Marca <code>&lt;references&gt;</code> inválida;
só o parâmetro "group" é permitido.
Utilize <code>&lt;references /&gt;</code>, ou <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Etiquetas de backlink esgotadas. 
Defina mais na mensagem <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_no_link_label_group' => 'Esgotamento das legendas personalizadas para links, no grupo "$1".
Defina mais na mensagem <nowiki>[[MediaWiki:$2]]</nowiki>.',
	'cite_error_references_no_text' => 'Marca <code>&lt;ref&gt;</code> inválida; 
não foi fornecido texto para as refs chamadas <code>$1</code>',
	'cite_error_included_ref' => '&lt;/ref&gt; de fechamento ausente para para a marca &lt;ref&gt;',
	'cite_error_refs_without_references' => 'existem marcas <code>&lt;ref&gt;</code>, mas nenhuma marca <code>&lt;references/&gt;</code> foi encontrada',
	'cite_error_group_refs_without_references' => 'existem marcas <code>&lt;ref&gt;</code> para um grupo chamado "$1", mas nenhuma marca <code>&lt;references group="$1"/&gt;</code> correspondente foi encontrada',
	'cite_error_references_group_mismatch' => 'marca <code>&lt;ref&gt;</code> em <code>&lt;references&gt;</code> tem o atributo grupo "$1" conflitante.',
	'cite_error_references_missing_group' => 'marca <code>&lt;ref&gt;</code> definida em <code>&lt;references&gt;</code> tem atributo grupo "$1" que não aparece no texto anterior.',
	'cite_error_references_missing_key' => 'marca <code>&lt;ref&gt;</code> com nome "$1" definida em <code>&lt;references&gt;</code> não foi utilizada no texto anterior.',
	'cite_error_references_no_key' => 'marca <code>&lt;ref&gt;</code> definida em <code>&lt;references&gt;</code> não tem atributo nome.',
	'cite_error_empty_references_define' => 'marca <code>&lt;ref&gt;</code> definida em <code>&lt;references&gt;</code> com nome "$1" não tem nenhum conteúdo.',
);

/** Quechua (Runa Simi)
 * @author AlimanRuna
 */
$messages['qu'] = array(
	'cite-desc' => 'Pukyumanta willanapaq <nowiki><ref[ name=id]></nowiki> , <nowiki><references/></nowiki> unanchachakunatam yapan',
	'cite_croak' => "''Cite'' nisqa mast'arinaqa manañam kanchu; $1: $2",
	'cite_error_key_str_invalid' => 'Ukhu pantasqa;
mana allin $str wan/icha $key.
Kayqa ama hayk\'appas tukukunchu.',
	'cite_error_stack_invalid_input' => "Ukhu pantasqa;
tawqa llawiqa manam allinchu.
Kayqa ama hayk'appas tukukunchu.",
	'cite_error' => 'Pukyumanta willaypi pantasqa: $1',
	'cite_error_ref_numeric_key' => "<code>&lt;ref&gt;</code> unanchachaqa manam allinchu;
sutinqa ama yupaylla kachunchu. Ch'uyanchaq sutinta llamk'achiy",
	'cite_error_ref_no_key' => "<code>&lt;ref&gt;</code> unanchachaqa manam allinchu;
ch'usaq pukyu willana unanchachaqa sutiyuqmi kachun",
	'cite_error_ref_too_many_keys' => '<code>&lt;ref&gt;</code> unanchachaqa manam allinchu;
sutinkunaqa manam allinchu, nisyu sutinchá',
	'cite_error_ref_no_input' => "<code>&lt;ref&gt;</code> unanchachaqa manam allinchu;
sutinnaq pukyu willana unanchachaqa ama ch'usaqchu kachun",
	'cite_error_references_invalid_parameters' => "<code>&lt;ref&gt;</code> unanchachaqa manam allinchu;
ama kuskanachina tupuchu kachun. <code>&lt;references /&gt;</code> unanchachata llamk'achiy",
	'cite_error_references_invalid_parameters_group' => '<code>&lt;ref&gt;</code> unanchachaqa manam allinchu;
"group" nisqa kuskanachina tupulla kachun. <code>&lt;references /&gt;</code> icha <code>&lt;references group="..." /&gt;</code> unanchachata llamk\'achiy',
	'cite_error_references_no_backlink_label' => "Manañam kanchu allichana yana t'inki unanchakuna.
Astawan sut'ichay <nowiki>[[MediaWiki:Cite references link many format backlink labels|Pukyumanta willaykuna achka allichana yana t'inki unanchakunata t'inkin]]</nowiki> nisqa willaypi",
	'cite_error_references_no_text' => '<code>&lt;ref&gt;</code> unanchachaqa manam allinchu;
<code>$1</code> nisqapaq pukyu qillqa manam kanchu',
	'cite_error_included_ref' => "Kichaq &lt;ref&gt; unanchachapaq wichq'aq &lt;/ref&gt; unanchachaqa manam kanchu",
	'cite_error_refs_without_references' => '<code>&lt;ref&gt;</code> unanchacham kachkan, ichataq manam <code>&lt;references/&gt;</code> unanchachachu',
	'cite_error_group_refs_without_references' => '"$1" sutiyuq huñupaq <code>&lt;ref&gt;</code> unanchacham kachkan, ichataq manam chay huñupaq qillqasqa <code>&lt;references/&gt;</code> unanchachachu',
);

/** Romanian (Română)
 * @author KlaudiuMihaila
 * @author Mihai
 */
$messages['ro'] = array(
	'cite-desc' => 'Adaugă etichete <nowiki><ref[ name=id]></nowiki> și <nowiki><references/></nowiki>, pentru citări',
	'cite_croak' => 'Citare coruptă; $1 : $2',
	'cite_error_key_str_invalid' => 'Eroare internă;
$str invalid sau/și $key.
Acestea nu ar trebui să se întâmple.',
	'cite_error_stack_invalid_input' => 'Eroare internă;
stivă cheie invalidă.
Acestea nu ar trebui să se întâmple.',
	'cite_error' => 'Eroare la citare: $1',
	'cite_error_ref_numeric_key' => 'Etichetă <code>&lt;ref&gt;</code> invalidă;
numele nu poate fi un număr. Folosește un titlu descriptiv',
	'cite_error_ref_no_key' => 'Etichetă <code>&lt;ref&gt;</code> invalidă;
ref-urile fără conținut trebuie să aibă un nume',
	'cite_error_ref_too_many_keys' => 'Etichetă <code>&lt;ref&gt;</code> invalidă;
nume invalid, ex. prea multe nume',
	'cite_error_ref_no_input' => 'Etichetă <code>&lt;ref&gt;</code> invalidă;
ref-urile fără nume trebuie să aibă conținut',
	'cite_error_references_invalid_parameters' => 'Etichetă <code>&lt;references&gt;</code> invalidă;
parametrii nu sunt permiși.
Folosește eticheta <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Etichetă <code>&lt;references&gt;</code> invalidă;
doar parametrul „grup” este permis.
Folosește eticheta <code>&lt;references /&gt;</code>, sau <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_text' => 'Etichetă <code>&lt;ref&gt;</code> invalidă;
niciun text nu a fost furnizat pentru ref-urile numite <code>$1</code>',
	'cite_error_included_ref' => 'Adaugă &lt;/ref&gt; eticheta a fost deschisă prin &lt;ref&gt;',
);

/** Tarandíne (Tarandíne)
 * @author Joetaras
 */
$messages['roa-tara'] = array(
	'cite-desc' => 'Aggiunge le tag <nowiki><ref[ name=id]></nowiki> and <nowiki><references/></nowiki> pe le citaziune',
	'cite_croak' => 'Cite muerte; $1: $2',
	'cite_error_key_str_invalid' => 'Errore inderne;
invalide $str e/o $key.
Quiste non g\'avessa succedere.',
	'cite_error_stack_invalid_input' => "Errore inderne;
stack key invalide.
Quiste non g'avessa succedere.",
	'cite_error' => 'Cite errore: $1',
	'cite_error_ref_numeric_key' => "Tag <code>&lt;ref&gt;</code> invalide;
'u nome non ge pò essere sole 'n'indere. Ause 'nu titele descrittive",
	'cite_error_ref_no_key' => "Tag <code>&lt;ref&gt;</code> invalide;
le referimende senza condenute onne tenè 'nu nome",
	'cite_error_ref_too_many_keys' => 'Tag <code>&lt;ref&gt;</code> invalide;
nome invalide, pe esembie troppe luènghe',
	'cite_error_ref_no_input' => "Tag <code>&lt;ref&gt;</code> invalide;
referimende senza nome onne tenè 'nu condenute",
	'cite_error_references_invalid_parameters' => 'Tag <code>&lt;references&gt;</code> invalide;
non ge se pò mettere nisciune parametre.
Ause <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Tag <code>&lt;references&gt;</code> invalide;
sulamende \'u parametre "group" pò essere ausate.
Ause <code>&lt;references /&gt;</code> o <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_text' => 'Tag <code>&lt;ref&gt;</code> invalide;
nisciune teste ere previste pe le referimende nnomenate <code>$1</code>',
	'cite_error_included_ref' => "Stè 'u tag &lt;/ref&gt; ma manghe &lt;ref&gt;",
	'cite_error_refs_without_references' => "'u tag <code>&lt;ref&gt;</code> esiste, ma non g'esiste 'u tag <code>&lt;references/&gt;</code>",
);

/** Russian (Русский)
 * @author Ahonc
 * @author Ferrer
 * @author Kaganer
 * @author Kalan
 * @author Александр Сигачёв
 */
$messages['ru'] = array(
	'cite-desc' => 'Добавляет теги <nowiki><ref[ name=id]></nowiki> и <nowiki><references/></nowiki> для сносок',
	'cite_croak' => 'Цитата сдохла; $1: $2',
	'cite_error_key_str_invalid' => 'Внутренняя ошибка;
ошибочное значение $str или $key.
Подобного никогда не должно произойти.',
	'cite_error_stack_invalid_input' => 'Внутренняя ошибка.
Неверный ключ стека.
Этого никогда не должно случиться.',
	'cite_error' => 'Ошибка цитирования $1',
	'cite_error_ref_numeric_key' => 'Неправильный тег <code>&lt;ref&gt;</code>;
имя не может быть целым числом. Используйте описательное название',
	'cite_error_ref_no_key' => 'Неправильный тег <code>&lt;ref&gt;</code>;
элемент без содержания должен иметь имя.',
	'cite_error_ref_too_many_keys' => 'Неправильный тег <code>&lt;ref&gt;</code>;
ошибочные имена, возможно, слишком много',
	'cite_error_ref_no_input' => 'Неправильный тег <code>&lt;ref&gt;</code>;
элемент без имени должен иметь содержание',
	'cite_error_references_invalid_parameters' => 'Неправильный тег <code>&lt;references&gt;</code>;
параметры не разрешены.
Используйте <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Ошибочный тег <code>&lt;references&gt;</code>;
можно использовать только параметр «group».
Используйте <code>&lt;references /&gt;</code>, или <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Не хватает символов для возвратных гиперссылок.
Следует расширить системное сообщение <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_no_link_label_group' => 'Закончились отметки пользовательских ссылок для группы «$1».
Определите дополнительные в сообщении <nowiki>[[MediaWiki:$2]]</nowiki>.',
	'cite_error_references_no_text' => 'Неверный тег <code>&lt;ref&gt;</code>; для сносок <code>$1</code> не указан текст',
	'cite_error_included_ref' => 'Отсутствует закрывающий тег &lt;/ref&gt;',
	'cite_error_refs_without_references' => 'Для существующего тега <code>&lt;ref&gt;</code> не найдено соответствующего тега <code>&lt;references/&gt;</code>',
	'cite_error_group_refs_without_references' => 'Для существующих тегов <code>&lt;ref&gt;</code> группы «$1» не найдено соответствующего тега <code>&lt;references group="$1"/&gt;</code>',
	'cite_error_references_group_mismatch' => 'Тег <code>&lt;ref&gt;</code> в <code>&lt;references&gt;</code> имеет конфликтующие группы атрибутов «$1».',
	'cite_error_references_missing_group' => 'Тег <code>&lt;ref&gt;</code>, определённый в <code>&lt;references&gt;</code>, имеет атрибут группы «$1», который не упоминается в тексте ранее.',
	'cite_error_references_missing_key' => 'Тег <code>&lt;ref&gt;</code> с именем «$1», определённый в <code>&lt;references&gt;</code>, не используется в предшествующем тексте.',
	'cite_error_references_no_key' => 'Тег <code>&lt;ref&gt;</code>, определённый в <code>&lt;references&gt;</code>, не имеет атрибута имени.',
	'cite_error_empty_references_define' => 'Тег <code>&lt;ref&gt;</code>, определённый в <code>&lt;references&gt;</code>, с именем «$1» не имеет содержания.',
	'cite_references_link_many_format_backlink_labels' => 'а б в г д е ё ж з и й к л м н о п р с т у ф х ц ч ш щ ъ ы ь э ю я аа аб ав аг ад ае ё аж аз аи ай ак ал ам ан ао ап ар ас ат ау аф ах ац ач аш ащ аъ аы аь аэ аю ая ба бб бв бг бд бе бж бз би бй бк бл бм бн бо бп бр бс бт бу бф бх бц бч бш бщ бъ бы бь бэ бю бя ва вб вв вг вд ве вж вз ви вй вк вл вм вн во вп вр вс вт ву вф вх вц вч вш вщ въ вы вь вэ вю вя га гб гв гг гд ге гж гз ги гй гк гл гм гн го гп гр гс гт гу гф гх гц гч гш гщ гъ гы гь гэ гю гя да дб дв дг дд де дж дз ди дй дк дл дм дн до дп др дс дт ду дф дх дц дч дш дщ дъ ды дь дэ дю дя еа еб ев ег ед ее еж ез еи ей ек ел ем ен ео еп ер ес ет еу еф ех ец еч еш ещ еъ еы еь еэ ею ея жа жб жв жг жд же жж жз жи жй жк жл жм жн жо жп жр жс жт жу жф жх жц жч жш жщ жъ жы жь жэ жю жя за зб зв зг зд зе зж зз зи зй зк зл зм зн зо зп зр зс зт зу зф зх зц зч зш зщ зъ зы зь зэ зю зя иа иб ив иг ид ие иж из ии ий ик ил им ин ио ип ир ис ит иу иф их иц ич иш ищ иъ иы иь иэ ию ия йа йб йв йг йд йе йж йз йи йй йк йл йм йн йо йп йр йс йт йу йф йх йц йч йш йщ йъ йы йь йэ йю йя ка кб кв кг кд ке кж кз ки кй кк кл км кн ко кп кр кс кт ку кф кх кц кч кш кщ къ кы кь кэ кю кя ла лб лв лг лд ле лж лз ли лй лк лл лм лн ло лп лр лс лт лу лф лх лц лч лш лщ лъ лы ль лэ лю ля ма мб мв мг мд ме мж мз ми мй мк мл мм мн мо мп мр мс мт му мф мх мц мч мш мщ мъ мы мь мэ мю мя на нб нв нг нд не нж нз ни нй нк нл нм нн но нп нр нс нт ну нф нх нц нч нш нщ нъ ны нь нэ ню ня оа об ов ог од ое ож оз ои ой ок ол ом он оо оп ор ос от оу оф ох оц оч ош ощ оъ оы оь оэ ою оя па пб пв пг пд пе пж пз пи пй пк пл пм пн по пп пр пс пт пу пф пх пц пч пш пщ пъ пы пь пэ пю пя ра рб рв рг рд ре рж рз ри рй рк рл рм рн ро рп рр рс рт ру рф рх рц рч рш рщ ръ ры рь рэ рю ря са сб св сг сд се сж сз си сй ск сл см сн со сп ср сс ст су сф сх сц сч сш сщ съ сы сь сэ сю ся та тб тв тг тд те тж тз ти тй тк тл тм тн то тп тр тс тт ту тф тх тц тч тш тщ тъ ты ть тэ тю тя уа уб ув уг уд уе уж уз уи уй ук ул ум ун уо уп ур ус ут уу уф ух уц уч уш ущ уъ уы уь уэ ую уя фа фб фв фг фд фе фж фз фи фй фк фл фм фн фо фп фр фс фт фу фф фх фц фч фш фщ фъ фы фь фэ фю фя ха хб хв хг хд хе хж хз хи хй хк хл хм хн хо хп хр хс хт ху хф хх хц хч хш хщ хъ хы хь хэ хю хя ца цб цв цг цд це цж цз ци цй цк цл цм цн цо цп цр цс цт цу цф цх цц цч цш цщ цъ цы ць цэ цю ця ча чб чв чг чд че чж чз чи чй чк чл чм чн чо чп чр чс чт чу чф чх чц чч чш чщ чъ чы чь чэ чю чя ша шб шв шг шд ше шж шз ши шй шк шл шм шн шо шп шр шс шт шу шф шх шц шч шш шщ шъ шы шь шэ шю шя ща щб щв щг щд ще щж щз щи щй щк щл щм щн що щп щр щс щт щу щф щх щц щч щш щщ щъ щы щь щэ щю щя ъа ъб ъв ъг ъд ъе ъж ъз ъи ъй ък ъл ъм ън ъо ъп ър ъс ът ъу ъф ъх ъц ъч ъш ъщ ъъ ъы ъь ъэ ъю ъя ыа ыб ыв ыг ыд ые ыж ыз ыи ый ык ыл ым ын ыо ып ыр ыс ыт ыу ыф ых ыц ыч ыш ыщ ыъ ыы ыь ыэ ыю ыя ьа ьб ьв ьг ьд ье ьж ьз ьи ьй ьк ьл ьм ьн ьо ьп ьр ьс ьт ьу ьф ьх ьц ьч ьш ьщ ьъ ьы ьь ьэ ью ья эа эб эв эг эд эе эж эз эи эй эк эл эм эн эо эп эр эс эт эу эф эх эц эч эш эщ эъ эы эь ээ эю эя юа юб юв юг юд юе юж юз юи юй юк юл юм юн юо юп юр юс ют юу юф юх юц юч юш ющ юъ юы юь юэ юю юя яа яб яв яг яд яе яж яз яи яй як ял ям ян яо яп яр яс ят яу яф ях яц яч яш ящ яъ яы яь яэ яю яя',
);

/** Rusyn (Русиньскый)
 * @author Gazeb
 */
$messages['rue'] = array(
	'cite-desc' => 'Придасть таґы <nowiki><ref[ name="id"]></nowiki> і&nbsp;<nowiki><references /></nowiki> на означіня цітацій',
	'cite_croak' => 'Нефункчна цітація; $1: $2',
	'cite_error_key_str_invalid' => 'Внутрїшня хыба; 
неплатный $str або $key. 
Тото бы не мало ниґда настати.',
	'cite_error_stack_invalid_input' => 'Внутрїшня хыба; 
неплатный ключ стека. 
Тото бы не мало ниґда настати.',
	'cite_error' => 'Хыбна цітація: $1',
	'cite_error_ref_numeric_key' => 'Хыба в таґу <code>&lt;ref&gt;</code>; назвов не сміє быти просте чісло, хоснуйте пописове означіня',
	'cite_error_ref_no_key' => 'Хыба в таґу <code>&lt;ref&gt;</code>; порожнї едітації мусять обсяговати назву',
	'cite_error_ref_too_many_keys' => 'Хыба в таґу <code>&lt;ref&gt;</code>; хыбны назвы, напр. є їх дуже много',
	'cite_error_ref_no_input' => 'Хыба в таґу <code>&lt;ref&gt;</code>; цітації без назвы мусять мати властный обсяг',
	'cite_error_references_invalid_parameters' => 'Хыба в таґу <code>&lt;references&gt;</code>;  ту не є доволеный параметер, хоснуйте <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Не платный таґ <tt>&lt;references&gt;</tt>;
є поволеный лем параметер „group“.
Хоснуйте <tt>&lt;references /&gt;</tt> або <tt>&lt;references group="..." /&gt;</tt>.',
	'cite_error_references_no_backlink_label' => 'Дішли означіня зворотных одказів, придайте їх пару до повідомлїня <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_no_link_label_group' => 'Дішли дефінованы значкы про ґрупу „$1“.
Звыште їх чісло у повідомлїню <nowiki>[[MediaWiki:$2]]</nowiki>.',
	'cite_error_references_no_text' => 'Хыба в таґу <code>&lt;ref&gt;</code>; цітації означеной <code>$1</code> не є доданый жаден текст',
	'cite_error_included_ref' => 'Хыбить закінчіня &lt;/ref&gt; k&nbsp;таґу &lt;ref&gt;',
	'cite_error_refs_without_references' => 'Найджена значка <code>&lt;ref&gt;</code> без одповідной значкы <code>&lt;references/&gt;</code>.',
	'cite_error_group_refs_without_references' => 'Найджена значка <code>&lt;ref&gt;</code> про ґрупу „$1“ без одповідной значкы <code>&lt;references group="$1"/&gt;</code>.',
	'cite_error_references_group_mismatch' => 'Значка <code>&lt;ref&gt;</code> внутрї <code>&lt;references&gt;</code> має дефіновану іншу ґрупу „$1“.',
	'cite_error_references_missing_group' => 'Значка <code>&lt;ref&gt;</code> внутрї <code>&lt;references&gt;</code> хоснує ґрупу „$1“, котра ся в попереднїм текстї не обявує.',
	'cite_error_references_missing_key' => 'На <code>&lt;ref&gt;</code> з іменом „$1“ дефінованый внутрї <code>&lt;references&gt;</code> не суть в попереднїм текстї жадны одказы.',
	'cite_error_references_no_key' => 'У значкы <code>&lt;ref&gt;</code> дефінованой внутрї <code>&lt;references&gt;</code> хыбить атрібут <code>name</code>.',
	'cite_error_empty_references_define' => 'У значкы <code>&lt;ref&gt;</code> з назвов „$1“ дефінованой внутрї <code>&lt;references&gt;</code> хыбить обсяг.',
);

/** Yakut (Саха тыла)
 * @author HalanTul
 */
$messages['sah'] = array(
	'cite-desc' => 'Хос быһаарыы <nowiki><ref[ name=id]></nowiki> уонна <nowiki><references/></nowiki> тиэктэрин эбэр',
	'cite_croak' => 'Быһа тардыы суох буолбут (Цитата сдохла); $1: $2',
	'cite_error_key_str_invalid' => 'Иһинээҕи сыыһа: $str уонна/эбэтэр $key сыыһалар.',
	'cite_error_stack_invalid_input' => 'Иһинээҕи сыыһа: stack key сыыһалаах',
	'cite_error' => 'Цитата сыыһата: $1',
	'cite_error_ref_numeric_key' => 'Неправильный вызов: ожидался нечисловой ключ',
	'cite_error_ref_no_key' => '<code>&lt;ref&gt;</code> тиэк алҕаһа (Неправильный вызов): аата (күлүүһэ) ыйыллыбатах',
	'cite_error_ref_too_many_keys' => '<code>&lt;ref&gt;</code> тиэк алҕаһа (Неправильный вызов): аата сыыһа ыйыллыбыт, эбэтэр наһаа элбэх аат суруллубут',
	'cite_error_ref_no_input' => '<code>&lt;ref&gt;</code> тиэк алҕастаах (Неверный вызов): иһинээҕитэ сыыһа',
	'cite_error_references_invalid_parameters' => 'Сыыһа параметрдар бэриллибиттэр; <code>&lt;references /&gt;</code> тиэккэ отой суох буолуохтаахтар',
	'cite_error_references_invalid_parameters_group' => 'Сыыһалаах <code>&lt;references&gt;</code> тиэк;
"group" эрэ парааматыры туһаныахха сөп.
Маны <code>&lt;references /&gt;</code>, эбэтэр <code>&lt;references group="..." /&gt;</code> туһан.',
	'cite_error_references_no_backlink_label' => 'Төннөрөр сигэлэргэ бэлиэлэрэ тиийбэттэр.
<nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki> диэн систиэмэ этиитин кэҥэтэн биэрэххэ наада',
	'cite_error_no_link_label_group' => '«$1» бөлөх кыттааччыларын сигэлэрин бэлиэлэрэ бүппүттэр.
Эбии манна <nowiki>[[MediaWiki:$2]]</nowiki> оҥор.',
	'cite_error_references_no_text' => 'Сыыһа <code>&lt;ref&gt;</code> тиэк (тег);
<code>$1</code> диэн хос быһаарыыларга аналлаах тиэкис суох',
	'cite_error_included_ref' => '&lt;/ref&gt; диэн сабар тиэк суох эбит',
	'cite_error_refs_without_references' => 'Баар <code>&lt;ref&gt;</code> тиэккэ сөп түбэһэр <code>&lt;references/&gt;</code> тиэк көстүбэтэ',
	'cite_error_group_refs_without_references' => '"$1" бөлөх <code>&lt;ref&gt;</code> тиэгигэр сөп түбэһэр <code>&lt;references group="$1"/&gt;</code> тиэк көстүбэтэ',
	'cite_error_references_group_mismatch' => '<code>&lt;references&gt;</code> туттуллар <code>&lt;ref&gt;</code> тиэк бэйэ бэйэлэрин кытта сөпсөспөт "$1" атрибуттаах бөлөхтөрдөөх',
	'cite_error_references_missing_group' => 'Бу <code>&lt;references&gt;</code> туттар маннык <code>&lt;ref&gt;</code> тиэгэ бөлөх тиэкиһигэр урут көрсүллүбэтэх "$1" атрибуттаах.',
	'cite_error_references_missing_key' => 'Бу <code>&lt;references&gt;</code> туттар маннык <code>&lt;ref&gt;</code> "$1" диэн тиэгэ бу иннинээҕи тиэкискэ туттуллубат эбит.',
	'cite_error_references_no_key' => 'Бу <code>&lt;references&gt;</code> туттар <code>&lt;ref&gt;</code> тиэгэ аатын атрибута суох эбит.',
	'cite_error_empty_references_define' => '<code>&lt;references&gt;</code> туттар <code>&lt;ref&gt;</code> "$1" диэн ааттаах тиэгэ иһинээҕитэ суох эбит.',
);

/** Sicilian (Sicilianu)
 * @author Melos
 * @author Santu
 */
$messages['scn'] = array(
	'cite-desc' => 'Junci li tag <nowiki><ref[ name=id]></nowiki> e <nowiki><references/></nowiki> pi gistiri li citazzioni',
	'cite_croak' => 'Sbàgghiu nnâ citazzioni: $1: $2',
	'cite_error_key_str_invalid' => 'Sbàgghiu nternu: $str sbagghiatu',
	'cite_error_stack_invalid_input' => 'Sbàgghiu nternu: chiavi di stack sbagghiata',
	'cite_error' => 'Sbàgghiu nnâ funzioni Cite $1',
	'cite_error_ref_numeric_key' => "Sbàgghiu ni l'usu dû marcaturi <code>&lt;ref&gt;</code>: lu nomu pò èssiri nu nùmmiru sanu. Usari nu tìtulu discrittivu",
	'cite_error_ref_no_key' => "Sbàgghiu ni l'usu dû marcaturi <code>&lt;ref&gt;</code>: li ref vacanti non ponnu èssiri senza nomu.",
	'cite_error_ref_too_many_keys' => "Sbàgghiu ni l'usu dû marcaturi <code>&lt;ref&gt;</code>: nomi non vàlidi (pi es. nùmmiru troppu àutu)",
	'cite_error_ref_no_input' => "Sbàgghiu ni l'usu dû marcaturi <code>&lt;ref&gt;</code>: li ref senza nomu non ponnu èssiri vacanti",
	'cite_error_references_invalid_parameters' => "Sbàgghiu ni l'usu dû marcaturi <code>&lt;references&gt;</code>: paràmitri non ammittuti, usari li marcaturi <code>&lt;references /&gt;</code>",
	'cite_error_references_invalid_parameters_group' => 'Sbàgghiu ni l\'usu dû marcaturi <code>&lt;references&gt;</code>; 
sulu lu paràmitru "group" è pirmittutu.
Usari <code>&lt;references /&gt;</code> oppuru <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Etichetti di rimannata pirsunalizzati finuti, aumintari lu nùmmiru ntô missàggiu <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_references_no_text' => 'Marcaturi <code>&lt;ref&gt;</code> non vàlidu; non vinni nnicatu nuddu testu pô marcaturi <code>$1</code>',
	'cite_error_included_ref' => '&lt;/ref&gt; di chiusura mancanti pô marcaturi &lt;ref&gt;',
	'cite_error_refs_without_references' => 'Sù prisenti dê marcatura <code>&lt;ref&gt;</code> ma nun fu attruvatu nissunu marcaturi <code>&lt;references/&gt;</code>',
	'cite_error_group_refs_without_references' => 'Sù prisenti dê marcatura <code>&lt;ref&gt;</code> pi nu gruppu chiamatu "$1" ma nun fu truvatu nissunu marcaturi <code>&lt;references group="$1"/&gt;</code> currispunnenti',
	'cite_error_references_group_mismatch' => 'Lu tag <code>&lt;ref&gt;</code> n <code>&lt;references&gt;</code> havi attribuiutu lu gruppu "$1" n conflittu.',
	'cite_error_references_missing_group' => 'Lu tag <code>&lt;ref&gt;</code> difinutu n <code>&lt;references&gt;</code> havi n\'attributu gruppu "$1" ca nun cumpari ni lu testu pricidenti.',
	'cite_error_references_missing_key' => 'Lu tag <code>&lt;ref&gt;</code> cu nomu "$1" difinutu n <code>&lt;references&gt;</code> nun è usatu nô testu pricidenti.',
	'cite_error_references_no_key' => "Lu tag <code>&lt;ref&gt;</code> difinutu n <code>&lt;references&gt;</code> nun havi n'attributu nomu.",
	'cite_error_empty_references_define' => 'Lu tag <code>&lt;ref&gt;</code> difinutu n <code>&lt;references&gt;</code> cu lu nomu "$1" nun havi alcun cuntinutu.',
	'cite_reference_link_prefix' => 'muntuari ref',
	'cite_references_link_prefix' => 'muntuari annutazzioni',
);

/** Sinhala (සිංහල)
 * @author නන්දිමිතුරු
 */
$messages['si'] = array(
	'cite-desc' => 'උපහරණයන් සඳහා, <nowiki><ref[ name=id]></nowiki> සහ <nowiki><references/></nowiki> ටැගයන්, එකතු කරයි',
	'cite_croak' => 'උපන්‍යාසය නිරුද්ධවිය; $1: $2',
	'cite_error_key_str_invalid' => 'අභ්‍යන්තර දෝෂය;
අනීතික  $str සහ/හෝ $key.
මෙය කිසිදින සිදුනොවිය යුතුය.',
	'cite_error_stack_invalid_input' => 'අභ්‍යන්තර දෝෂය;
අනීතික ඇසිරුම්  යතුර.
මෙය කිසිදින සිදුනොවිය යුතුය.',
	'cite_error' => 'උපන්‍යාස දෝෂය: $1',
	'cite_error_ref_numeric_key' => 'අනීතික <code>&lt;ref&gt;</code> ටැගය;
නම සරල  පූර්ණාංකයක් විය නොහැක. විස්තරශීලි ශිර්ෂයක් භාවිතා කරන්න',
	'cite_error_ref_no_key' => 'අනීතික <code>&lt;ref&gt;</code> ටැගය;
පෙළ විරහිත ආශ්‍රේය සඳහා නමක් තිබිය යුතුය',
	'cite_error_ref_too_many_keys' => 'අනීතික <code>&lt;ref&gt;</code> ටැගය;
අනීතික නාමයන්, නිද. පමණට වැඩි',
	'cite_error_ref_no_input' => 'අනීතික <code>&lt;ref&gt;</code> ටැගය;
නාමයක් නොමැති ආශ්‍රේය සඳහා පෙළක් තිබිය යුතුය',
	'cite_error_references_invalid_parameters' => 'අනීතික <code>&lt;references&gt;</code> ටැගය;
කිසිම පරාමිතිකයකට ඉඩ නොදෙයි.
<code>&lt;references /&gt;</code> භාවිත කරන්න',
	'cite_error_references_invalid_parameters_group' => 'අනීතික <code>&lt;references&gt;</code> ටැගය;
"කාණ්ඩය" පරාමිතියට පමණක් ඉඩ දෙයි.
<code>&lt;references /&gt;</code>, හෝ <code>&lt;references group="..." /&gt;</code> භාවිත කරන්න',
	'cite_error_references_no_backlink_label' => 'උපයෝග්‍ය පසුසබැඳුම් ලේබල අවසාන විය.
<nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki> පණිවුඩයෙහි තවත් ඒවා අර්ථදක්වන්න',
	'cite_error_references_no_text' => 'අනීතික <code>&lt;ref&gt;</code> ටැගය;
<code>$1</code> නමැති ආශ්‍රේයන් සඳහා කිසිදු පෙළක් සපයා නොතිබුණි',
	'cite_error_included_ref' => '&lt;ref&gt; ටැගය සොයාගත නොහැකි බැවින් &lt;/ref&gt; වසා දමමින්',
	'cite_reference_link_prefix' => 'උපන්‍යාස_යොමුව-',
	'cite_references_link_prefix' => 'උපන්‍යාස_සටහන-',
);

/** Slovak (Slovenčina)
 * @author Helix84
 */
$messages['sk'] = array(
	'cite-desc' => 'Pridáva značky <nowiki><ref[ name=id]></nowiki> a <nowiki><references/></nowiki> pre citácie',
	'cite_croak' => 'Citát je už neaktuálny; $1: $2',
	'cite_error_key_str_invalid' => 'Vnútorná chyba;
neplatný $str a/alebo $key.
Toto by sa nemalo nikdy stať.',
	'cite_error_stack_invalid_input' => 'Vnútorná chyba; neplatný kľúč zásobníka',
	'cite_error' => 'Chyba citácie $1',
	'cite_error_ref_numeric_key' => 'Neplatné volanie; očakáva sa neceločíselný typ kľúča',
	'cite_error_ref_no_key' => 'Neplatné volanie; nebol špecifikovaný kľúč',
	'cite_error_ref_too_many_keys' => 'Neplatné volanie; neplatné kľúče, napr. príliš veľa alebo nesprávne špecifikovaný kľúč',
	'cite_error_ref_no_input' => 'Neplatné volanie; nebol špecifikovaný vstup',
	'cite_error_references_invalid_parameters' => 'Neplatné parametre; neočakávli sa žiadne',
	'cite_error_references_invalid_parameters_group' => 'Neplatná značka <code>&lt;references&gt;</code>;
je povolený iba parameter „group“.
Použite <code>&lt;references /&gt;</code> alebo <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Minuli sa generované návestia spätných odkazov, definujte viac v správe <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_no_link_label_group' => 'Vyčerpané prispôsobené označenia odkazov pre skupinu „$1“.
Definujte ďalšie v správe <nowiki>[[MediaWiki:$2]]</nowiki>.',
	'cite_error_references_no_text' => 'Neplatná značka <code>&lt;ref&gt;</code>; nebol zadaný text pre referencie s názvom <code>$1</code>',
	'cite_error_included_ref' => 'Chýba zakončenie značky &lt;ref&gt; (&lt;/ref&gt;)',
	'cite_error_refs_without_references' => 'Značky <code>&lt;ref&gt;</code> sú prítomné, ale nebola nájdená žiadna značka <code>&lt;references/&gt;</code>',
	'cite_error_group_refs_without_references' => 'Značky <code>&lt;ref&gt;</code> pre skupinu „$1“ sú prítomné, ale nebola nájdená zodpovedajúca značka  <code>&lt;references group="$1"/&gt;</code>',
	'cite_error_references_group_mismatch' => 'Značka <code>&lt;ref&gt;</code> v <code>&lt;references&gt;</code> má konfliktný atribút skupiny „$1“.',
	'cite_error_references_missing_group' => 'Značka <code>&lt;ref&gt;</code> v <code>&lt;references&gt;</code> má atribút skupiny „$1“, ktorý sa v predošlom texte nevyskytuje.',
	'cite_error_references_missing_key' => 'Značka <code>&lt;ref&gt;</code> s názvom „$1“ definovaná v <code>&lt;references&gt;</code> sa v predošlom texte nevyskytuje.',
	'cite_error_references_no_key' => 'Značka <code>&lt;ref&gt;</code> s definovaná v <code>&lt;references&gt;</code> nemá žiaden atribút názov.',
	'cite_error_empty_references_define' => 'Značka <code>&lt;ref&gt;</code> s definovaná v <code>&lt;references&gt;</code> s názvom „$1“ nemá žiaden obsah.',
);

/** Slovenian (Slovenščina)
 * @author Dbc334
 */
$messages['sl'] = array(
	'cite-desc' => 'Doda etiketi <nowiki><ref[ name=id]></nowiki> in <nowiki><references/></nowiki> za navajanje',
	'cite_croak' => 'Hreščeča navedba; $1: $2',
	'cite_error_key_str_invalid' => 'Notranja napaka;
neveljaven $str in/ali $key.
To se ne bi nikoli smelo zgoditi.',
	'cite_error_stack_invalid_input' => 'Notranja napaka;
neveljavni skladovni ključ.
To se ne bi nikoli smelo zgoditi.',
	'cite_error' => 'Napaka pri navajanju: $1',
	'cite_error_ref_numeric_key' => 'Neveljavna oznaka <code>&lt;ref&gt;</code>;
ime ne more biti samo število. Uporabite opisni naslov',
	'cite_error_ref_no_key' => 'Neveljavna oznaka <code>&lt;ref&gt;</code>;
sklici brez vsebine morajo imeti ime',
	'cite_error_ref_too_many_keys' => 'Neveljavna etiketa <code>&lt;ref&gt;</code>;
neveljavna imena, npr. preveč',
	'cite_error_ref_no_input' => 'Neveljavna oznaka <code>&lt;ref&gt;</code>;
sklici brez imena morajo imeti vsebino',
	'cite_error_references_invalid_parameters' => 'Neveljavna etiketa <code>&lt;references&gt;</code>;
parametri niso dovoljeni.
Uporabite <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Neveljavna etiketa <code>&lt;references&gt;</code>;
dovoljen je samo parameter »group«.
Uporabite <code>&lt;references /&gt;</code> ali <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Zmanjkalo je oznak za povratne povezave.
Določite jih več v sporočilu <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>.',
	'cite_error_no_link_label_group' => 'Zmanjkalo je oznak povezav po meri za skupino »$1«.
Določite jih več v sporočilu <nowiki>[[MediaWiki:$2]]</nowiki>.',
	'cite_error_references_no_text' => 'Neveljavna oznaka <code>&lt;ref&gt;</code>;
sklici imenovani <code>$1</code> nimajo določenega besedila',
	'cite_error_included_ref' => 'Zaključek &lt;/ref&gt; manjka za etiketo &lt;ref&gt;',
	'cite_error_refs_without_references' => 'Etiketa <code>&lt;ref&gt;</code> obstaja, vendar etiketa <code>&lt;references/&gt;</code> ni bila najdena',
	'cite_error_group_refs_without_references' => 'Etiketa <code>&lt;ref&gt;</code> obstaja za skupino imenovano »$1«, vendar ustrezna etiketa <code>&lt;references group="$1"/&gt;</code> ni bila najdena',
	'cite_error_references_group_mismatch' => 'Oznaka <code>&lt;ref&gt;</code> v <code>&lt;references&gt;</code> ima atribut nasprotujoče si skupine »$1«.',
	'cite_error_references_missing_group' => 'Oznaka <code>&lt;ref&gt;</code>, opredeljena v <code>&lt;references&gt;</code>, ima atribut skupine »$1«, ki se ne pojavi v predhodnem besedilu.',
	'cite_error_references_missing_key' => 'Oznaka <code>&lt;ref&gt;</code> z imenom »$1«, opredeljena v <code>&lt;references&gt;</code>, ni uporabljena v predhodnem besedilu.',
	'cite_error_references_no_key' => 'Etiketa <code>&lt;ref&gt;</code>, določena v <code>&lt;references&gt;</code>, nima lastnosti »name«.',
	'cite_error_empty_references_define' => 'Etiketa <code>&lt;ref&gt;</code>, določena v <code>&lt;references&gt;</code> z imenom »$1«, nima vsebine.',
	'cite_references_link_one' => "<li id=\"\$1\">'''[[#\$2|^]]''' \$3</li>",
	'cite_references_link_many' => '<li id="$1">^ $2 $3</li>',
	'cite_references_link_many_format_backlink_labels' => 'a b c č d e f g h i j k l m n o p r s š t u v z ž a aa ab ac ač ad ae af ag ah ai aj ak al am an ao ap ar as aš at au av az až b ba bb bc bč bd be bf bg bh bi bj bk bl bm bn bo bp br bs bš bt bu bv bz bž c ca cb cc cč cd ce cf cg ch ci cj ck cl cm cn co cp cr cs cš ct cu cv cz cž č ča čb čc čč čd če čf čg čh či čj čk čl čm čn čo čp čr čs čš čt ču čv čz čž d da db dc dč dd de df dg dh di dj dk dl dm dn do dp dr ds dš dt du dv dz dž e ea eb ec eč ed ee ef eg eh ei ej ek el em en eo ep er es eš et eu ev ez ež f fa fb fc fč fd fe ff fg fh fi fj fk fl fm fn fo fp fr fs fš ft fu fv fz fž g ga gb gc gč gd ge gf gg gh gi gj gk gl gm gn go gp gr gs gš gt gu gv gz gž h ha hb hc hč hd he hf hg hh hi hj hk hl hm hn ho hp hr hs hš ht hu hv hz hž i ia ib ic ič id ie if ig ih ii ij ik il im in io ip ir is iš it iu iv iz iž j ja jb jc jč jd je jf jg jh ji jj jk jl jm jn jo jp jr js jš jt ju jv jz jž k ka kb kc kč kd ke kf kg kh ki kj kk kl km kn ko kp kr ks kš kt ku kv kz kž l la lb lc lč ld le lf lg lh li lj lk ll lm ln lo lp lr ls lš lt lu lv lz lž m ma mb mc mč md me mf mg mh mi mj mk ml mm mn mo mp mr ms mš mt mu mv mz mž n na nb nc nč nd ne nf ng nh ni nj nk nl nm nn no np nr ns nš nt nu nv nz nž o oa ob oc oč od oe of og oh oi oj ok ol om on oo op or os oš ot ou ov oz ož p pa pb pc pč pd pe pf pg ph pi pj pk pl pm pn po pp pr ps pš pt pu pv pz pž r ra rb rc rč rd re rf rg rh ri rj rk rl rm rn ro rp rr rs rš rt ru rv rz rž s sa sb sc sč sd se sf sg sh si sj sk sl sm sn so sp sr ss sš st su sv sz sž š ša šb šc šč šd še šf šg šh ši šj šk šl šm šn šo šp šr šs šš št šu šv šz šž t ta tb tc tč td te tf tg th ti tj tk tl tm tn to tp tr ts tš tt tu tv tz tž u ua ub uc uč ud ue uf ug uh ui uj uk ul um un uo up ur us uš ut uu uv uz už v va vb vc vč vd ve vf vg vh vi vj vk vl vm vn vo vp vr vs vš vt vu vv vz vž z za zb zc zč zd ze zf zg zh zi zj zk zl zm zn zo zp zr zs zš zt zu zv zz zž ž ža žb žc žč žd že žf žg žh ži žj žk žl žm žn žo žp žr žs žš žt žu žv žz žž',
);

/** Serbian Cyrillic ekavian (Српски (ћирилица))
 * @author Millosh
 * @author Rancher
 * @author Михајло Анђелковић
 */
$messages['sr-ec'] = array(
	'cite-desc' => 'Додаје <nowiki><ref[ name=id]></nowiki> и <nowiki><references/></nowiki> ознаке за цитирање.',
	'cite_croak' => 'Додатак за цитирање је неисправан; $1: $2.',
	'cite_error_key_str_invalid' => 'Унутрашња грешка; лош $str и/или $key. Ово не би требало никад да се деси.',
	'cite_error_stack_invalid_input' => 'Унутрашња грешка; лош кључ стека. Ово не би требало никад да се деси.',
	'cite_error' => 'Грешка цитата: $1',
	'cite_error_ref_numeric_key' => 'Лоша ознака <code>&lt;ref&gt;</code>; име не може бити једноставни интеџер. Користи описни наслов.',
	'cite_error_ref_no_key' => 'Лоша ознака <code>&lt;ref&gt;</code>; ref-ови без садржаја морају имати име.',
	'cite_error_ref_too_many_keys' => 'Лоша ознака <code>&lt;ref&gt;</code>; лоша имена, односно много њих.',
	'cite_error_ref_no_input' => 'Лоша ознака <code>&lt;ref&gt;</code>; ref-ови без имена морају имати садржај.',
	'cite_error_references_invalid_parameters' => 'Лоша ознака <code>&lt;references&gt;</code>; параметри нису дозвољени. Користи <code>&lt;references /&gt;</code>.',
	'cite_error_references_invalid_parameters_group' => 'Лоша ознака <code>&lt;references&gt;</code>; само је парамтера "group" дозвољен. Користи <code>&lt;references /&gt;</code> или <code>&lt;references group="..."&gt;</code>.',
	'cite_error_references_no_backlink_label' => 'Нестале су посебне ознаке за задње везе. Одреди их више у поруци <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>.',
	'cite_error_references_no_text' => 'Лоша ознака <code>&lt;ref&gt;</code>; нема текста за ref-ове под именом <code>$1</code>.',
	'cite_error_included_ref' => 'Затвара &lt;/ref&gt; који недостаје &lt;ref&gt; тагу',
	'cite_error_refs_without_references' => '<code>&lt;ref&gt;</code> таг постоји, али одговарајући <code>&lt;references/&gt;</code> таг није нађен',
	'cite_error_group_refs_without_references' => '<code>&lt;ref&gt;</code> тагови постоје за групу под именом "$1", али није нађен одговарајући <code>&lt;references group="$1"/&gt;</code> таг',
);

/** Serbian Latin ekavian (Srpski (latinica))
 * @author Michaello
 */
$messages['sr-el'] = array(
	'cite-desc' => 'Dodaje <nowiki><ref[ name=id]></nowiki> i <nowiki><references/></nowiki> oznake za citiranje.',
	'cite_croak' => 'Dodatak za citiranje je umro; $1: $2.',
	'cite_error_key_str_invalid' => 'Unutrašnja greška; loš $str i/ili $key. Ovo ne bi trebalo nikad da se desi.',
	'cite_error_stack_invalid_input' => 'Unutrašnja greška; loš ključ steka. Ovo ne bi trebalo nikad da se desi.',
	'cite_error' => 'Greška citata: $1',
	'cite_error_ref_numeric_key' => 'Loša oznaka <code>&amp;lt;ref&amp;gt;</code>; ime ne može biti jednostavni intedžer. Koristi opisni naslov.',
	'cite_error_ref_no_key' => 'Loša oznaka <code>&amp;lt;ref&amp;gt;</code>; ref-ovi bez sadržaja moraju imati ime.',
	'cite_error_ref_too_many_keys' => 'Loša oznaka <code>&amp;lt;ref&amp;gt;</code>; loša imena, odnosno mnogo njih.',
	'cite_error_ref_no_input' => 'Loša oznaka <code>&amp;lt;ref&amp;gt;</code>; ref-ovi bez imena moraju imati sadržaj.',
	'cite_error_references_invalid_parameters' => 'Loša oznaka <code>&amp;lt;references&amp;gt;</code>; parametri nisu dozvoljeni. Koristi <code>&amp;lt;references /&amp;gt;</code>.',
	'cite_error_references_invalid_parameters_group' => 'Loša oznaka <code>&amp;lt;references&amp;gt;</code>; samo je paramtera &quot;group&quot; dozvoljen. Koristi <code>&amp;lt;references /&amp;gt;</code> ili <code>&amp;lt;references group=&quot;...&quot;&amp;gt;</code>.',
	'cite_error_references_no_backlink_label' => 'Nestale su posebne oznake za zadnje veze. Odredi ih više u poruci <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>.',
	'cite_error_references_no_text' => 'Loša oznaka <code>&amp;lt;ref&amp;gt;</code>; nema teksta za ref-ove pod imenom <code>$1</code>.',
	'cite_error_included_ref' => 'Zatvara &amp;lt;/ref&amp;gt; koji nedostaje &amp;lt;ref&amp;gt; tagu',
	'cite_error_refs_without_references' => '<code>&amp;lt;ref&amp;gt;</code> tag postoji, ali odgovarajući <code>&amp;lt;references/&amp;gt;</code> tag nije nađen',
	'cite_error_group_refs_without_references' => '<code><ref></code> tagovi postoje za grupu pod imenom "$1", ali nije nađen odgovarajući <code><references group="$1"/></code> tag',
);

/** Seeltersk (Seeltersk)
 * @author Pyt
 */
$messages['stq'] = array(
	'cite-desc' => 'Föiget foar Wällenätterwiese do <nowiki><ref[ name=id]></nowiki> un <nowiki><references/></nowiki> Tags tou',
	'cite_croak' => 'Failer in dät Referenz-System. $1: $2',
	'cite_error_key_str_invalid' => 'Internen Failer: uungultigen $str un/of $key. Dit schuul eegentelk goar nit passierje konne.',
	'cite_error_stack_invalid_input' => 'Internen Failer: uungultigen „name“-stack. Dit schuul eegentelk goarnit passierje konne.',
	'cite_error' => 'Referenz-Failer $1',
	'cite_error_ref_numeric_key' => 'Uungultige <code><nowiki><ref></nowiki></code>-Ferweendenge: „name“ duur naan scheenen Taalenwäid weese, benutsje n beschrieuwenden Noome.',
	'cite_error_ref_no_key' => 'Uungultige <code><nowiki><ref></nowiki></code>-Ferweendenge: „ref“ sunner Inhoold mout n Noome hääbe.',
	'cite_error_ref_too_many_keys' => 'Uungultige <code><nowiki><ref></nowiki></code>-Ferweendenge: „name“ is uungultich of tou loang.',
	'cite_error_ref_no_input' => 'Uungultige <code><nowiki><ref></nowiki></code>-Ferweendenge: „ref“ sunner Noome mout n Inhoold hääbe.',
	'cite_error_references_invalid_parameters' => 'Uungultige <code><nowiki><reference></nowiki></code>-Ferweendenge: Der sunt neen bietoukuumende Parametere ferlööwed, ferweend bloot <code><nowiki><reference /></nowiki></code>.',
	'cite_error_references_invalid_parameters_group' => 'Ungultige <code>&lt;references&gt;</code>-Ferweendenge: Bloot die Parameter „group“ is ferlööwed, ferweend <tt>&lt;references /&gt;</tt> of <tt>&lt;references group="..." /&gt;</tt>',
	'cite_error_references_no_backlink_label' => 'Ne Referenz fon ju Foarm <code><nowiki><ref name="..."/></nowiki></code> wäd oafter benutsed as Bouksteeuwen deer sunt. N Administrator mout <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki> uum wiedere Bouksteeuwen/Teekene ferfulständigje.',
	'cite_error_references_no_text' => 'Uungultigen <code>&lt;ref&gt;</code>-Tag; der wuude naan Text foar dät Ref mäd dän Noome <code>$1</code> anroat.',
	'cite_error_included_ref' => 'Der failt n sluutend &lt;/ref&gt;',
	'cite_error_refs_without_references' => '<code>&lt;ref&gt;</code>-Tags existierje, daach neen <code>&lt;references/&gt;</code>-Tag wuud fuunen.',
	'cite_error_group_refs_without_references' => '<code>&lt;ref&gt;</code>-Tags existierje foar ju Gruppe „$1“, man neen deertou heerend <code>&lt;references group=„$1“/&gt;</code>-Tag wuud fuunen',
	'cite_error_references_group_mismatch' => 'Dät <code>&lt;ref&gt;</code>-Tag in <code>&lt;references&gt;</code> häd dät Konfliktgruppenattribut „$1“.',
	'cite_error_references_missing_group' => 'Dät <code>&lt;ref&gt;</code> Tag, as definierd in <code>&lt;references&gt;</code> häd dät Gruppenattribut "$1", dät nit in dän foaruutgungende Text foarkumt.',
	'cite_error_references_missing_key' => 'Dät in <code>&lt;references&gt;</code> definierde <code>&lt;ref&gt;</code>-Tag mäd dän Noome „$1“ wäd in dän foaruutgungende Text nit ferwoand.',
	'cite_error_references_no_key' => 'Dät in <code>&lt;references&gt;</code> definierde <code>&lt;ref&gt;</code>-Tag häd neen Noomensattribut.',
	'cite_error_empty_references_define' => 'Dät in <code>&lt;references&gt;</code> definierde <code>&lt;ref&gt;</code>-Tag mäd dän Noome „$1“ wiest naan Inhoold ap.',
);

/** Sundanese (Basa Sunda)
 * @author Kandar
 */
$messages['su'] = array(
	'cite-desc' => 'Nambahkeun tag <nowiki><ref[ name=id]></nowiki> jeung <nowiki><references/></nowiki>, pikeun cutatan',
	'cite_error_key_str_invalid' => 'Kasalahan internal; salah $str jeung/atawa $key. Kuduna mah teu kieu.',
);

/** Swedish (Svenska)
 * @author Boivie
 * @author Fluff
 * @author Lejonel
 * @author M.M.S.
 * @author Najami
 */
$messages['sv'] = array(
	'cite-desc' => 'Lägger till taggarna <nowiki><ref[ name=id]></nowiki> och <nowiki><references/></nowiki> för referenser till källor',
	'cite_croak' => 'Fel i fotnotssystemet; $1: $2',
	'cite_error_key_str_invalid' => 'Internt fel; $str eller $key är ogiltiga.  Det här borde aldrig hända.',
	'cite_error_stack_invalid_input' => 'Internt fel; ogiltig nyckel i stacken.  Det här borde aldrig hända.',
	'cite_error' => 'Referensfel: $1',
	'cite_error_ref_numeric_key' => "Ogiltig <code>&lt;ref&gt;</code>-tag; parametern 'name' kan inte vara ett tal, använd ett beskrivande namn",
	'cite_error_ref_no_key' => 'Ogiltig <code>&lt;ref&gt;</code>-tag; referenser utan innehåll måste ha ett namn',
	'cite_error_ref_too_many_keys' => "Ogiltig <code>&lt;ref&gt;</code>-tag; ogiltiga parametrar, den enda tillåtna parametern är 'name'",
	'cite_error_ref_no_input' => 'Ogiltig <code>&lt;ref&gt;</code>-tag; referenser utan namn måste ha innehåll',
	'cite_error_references_invalid_parameters' => 'Ogiltig <code>&lt;references&gt;</code>-tag; inga parametrar tillåts, använd <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Ogiltig <code>&lt;references&gt;</code>-tagg;
"group"-parametern är endast tillåten.
Använd <code>&lt;references /&gt;</code>, eller <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'De definierade etiketterna för tillbaka-länkar har tagit slut, definiera fler etiketter i systemmedelandet <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_no_link_label_group' => 'Anpassade länketiketter för gruppen "$1" tog slut.
Definera fler i <nowiki>[[MediaWiki:$2]]</nowiki>-meddelandet.',
	'cite_error_references_no_text' => 'Ogiltig <code>&lt;ref&gt;</code>-tag; ingen text har angivits för referensen med namnet <code>$1</code>',
	'cite_error_included_ref' => 'Avslutande &lt;/ref&gt; saknas för &lt;ref&gt;-tagg',
	'cite_error_refs_without_references' => '<code>&lt;ref&gt;</code>-taggar finns, men ingen <code>&lt;references/&gt;</code>-tagg hittades',
	'cite_error_group_refs_without_references' => '<code>&lt;ref&gt;</code>-taggar finns för gruppnamnet "$1", men ingen motsvarande <code>&lt;references group="$1"/&gt;</code>-tagg hittades',
	'cite_error_references_group_mismatch' => '<code>&lt;ref&gt;</code>-tagg i <code>&lt;references&gt;</code> har ett motstridigt group-attribut "$1".',
	'cite_error_references_missing_group' => '<code>&lt;ref&gt;</code>-tagg definierad i  <code>&lt;references&gt;</code> har ett group-attribut "$1" som inte används innan i texten.',
	'cite_error_references_missing_key' => '<code>&lt;ref&gt;</code>-tagg med namnet "$1", definierad i <code>&lt;references&gt;</code> används inte innan i texten.',
	'cite_error_references_no_key' => '<code>&lt;ref&gt;</code>-tagg definierad i <code>&lt;references&gt;</code> saknar name-attribut.',
	'cite_error_empty_references_define' => '<code>&lt;ref&gt;</code>-tagg definierad i <code>&lt;ref&gt;</code> med namnet "$1" har inget innehåll.',
);

/** Telugu (తెలుగు)
 * @author Chaduvari
 * @author Veeven
 */
$messages['te'] = array(
	'cite-desc' => 'ఉదహరింపులకు <nowiki><ref[ name=id]></nowiki> మరియు <nowiki><references/></nowiki> టాగులను చేర్చుతుంది',
	'cite_croak' => 'ఉదహరింపు చచ్చింది; $1: $2',
	'cite_error_key_str_invalid' => 'అంతర్గత పొరపాటు: తప్పుడు $str మరియు/లేదా $key. ఇది ఎప్పుడూ జరగకూడదు.',
	'cite_error_stack_invalid_input' => 'అంతర్గత పొరపాటు: తప్పుడు స్టాక్ కీ. ఇది ఎప్పుడూ జరగకూడదు.',
	'cite_error' => 'ఉదహరింపు పొరపాటు: $1',
	'cite_error_ref_numeric_key' => 'తప్పుడు <code>&lt;ref&gt;</code> టాగు; పేరు సరళ సంఖ్య అయివుండకూడదు, వివరమైన శీర్షిక వాడండి',
	'cite_error_ref_no_key' => 'సరైన <code>&lt;ref&gt;</code> ట్యాగు కాదు; విషయం లేని ref లకు తప్పనిసరిగా పేరొకటుండాలి',
	'cite_error_ref_too_many_keys' => 'సరైన <code>&lt;ref&gt;</code> ట్యాగు కాదు; తప్పు పేర్లు, ఉదాహరణకు మరీ ఎక్కువ',
	'cite_error_ref_no_input' => 'సరైన <code>&lt;ref&gt;</code> ట్యాగు కాదు; పేరు లేని ref లలో తప్పనిసరిగా విషయం ఉండాలి',
	'cite_error_references_invalid_parameters' => 'సరైన <code>&lt;references&gt;</code> ట్యాగు కాదు; పారామీటర్లకు కు అనుమతి లేదు, ఈ లోపాన్ని కలుగజేసే ఒక ఉదాహరణ: <references someparameter="value" />',
	'cite_error_references_no_backlink_label' => 'మీ స్వంత బ్యాక్‌లింకు లేబుళ్ళు అయిపోయాయి. <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki> సందేశంలో మరిన్ని లేబుళ్ళను నిర్వచించుకోండి.',
	'cite_error_references_no_text' => 'సరైన <code>&lt;ref&gt;</code> కాదు; <code>$1</code> అనే పేరుగల ref లకు పాఠ్యమేమీ ఇవ్వలేదు',
);

/** Tajik (Cyrillic) (Тоҷикӣ (Cyrillic))
 * @author Ibrahim
 */
$messages['tg-cyrl'] = array(
	'cite-desc' => 'Барчасбҳои <nowiki><ref[ name=id]></nowiki> ва <nowiki><references/></nowiki>  барои ёд кардан, изофа мекунад',
	'cite_croak' => 'Ёд кардан хароб шуд; $1: $2',
	'cite_error_key_str_invalid' => 'Хатои дохилӣ; $str ва/ё $key ғайримиҷоз.  Ин хато набояд ҳаргиз рух диҳад.',
	'cite_error_stack_invalid_input' => 'Хатои дохилӣ; клиди пушта ғайримиҷоз.  Ин хато набояд ҳаргиз рух диҳад.',
	'cite_error' => 'Хатои ёдкард: $1',
	'cite_error_ref_numeric_key' => 'Барчасби <code>&lt;ref&gt;</code> ғайримиҷоз; ном наметавонад як адад бошад, унвони возеҳтареро истифода кунед',
	'cite_error_ref_no_key' => 'Барчасби <code>&lt;ref&gt;</code> ғайримиҷоз; ёдкардҳо бидуни мӯҳтаво бояд ном дошта бошанд',
	'cite_error_ref_too_many_keys' => 'Барчасби  <code>&lt;ref&gt;</code> ғайримиҷоз; номҳои ғайримиҷоз ё беш аз андоза',
	'cite_error_ref_no_input' => 'Барчасби  <code>&lt;ref&gt;</code> ғайримиҷоз; ёдкардҳои бидуни ном бояд мӯҳтаво дошта бошанд',
	'cite_error_references_invalid_parameters' => 'Барчасби <code>&lt;references&gt;</code> ғайримиҷоз; истифода аз параметр миҷоз аст, аз  <code>&lt;references /&gt;</code> истифода кунед',
	'cite_error_references_invalid_parameters_group' => 'Барчасби <code>&lt;references&gt;</code> номӯътабар;
параметри "гурӯҳ" танҳо иҷозашуда аст.
Барчасби <code>&lt;references /&gt;</code> ё <code>&lt;references group="..." /&gt;</code> -ро истифода баред',
	'cite_error_references_no_backlink_label' => 'Барчасбҳои пайванд ба интиҳо расид, мавориди ҷадидро дар пайём  <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki> истифода кунед',
	'cite_error_references_no_text' => 'Барчасби  <code>&lt;ref&gt;</code> ғайримиҷоз; матне барои ёдкардҳо бо номи <code>$1</code> ворид нашудааст',
);

/** Tajik (Latin) (Тоҷикӣ (Latin))
 * @author Liangent
 */
$messages['tg-latn'] = array(
	'cite-desc' => 'Barcasbhoi <nowiki><ref[ name=id]></nowiki> va <nowiki><references/></nowiki>  baroi jod kardan, izofa mekunad',
	'cite_croak' => 'Jod kardan xarob şud; $1: $2',
	'cite_error_key_str_invalid' => 'Xatoi doxilī; $str va/jo $key ƣajrimiçoz.  In xato nabojad hargiz rux dihad.',
	'cite_error_stack_invalid_input' => 'Xatoi doxilī; klidi puşta ƣajrimiçoz.  In xato nabojad hargiz rux dihad.',
	'cite_error' => 'Xatoi jodkard: $1',
	'cite_error_ref_numeric_key' => 'Barcasbi <code>&lt;ref&gt;</code> ƣajrimiçoz; nom nametavonad jak adad boşad, unvoni vozehtarero istifoda kuned',
	'cite_error_ref_no_key' => 'Barcasbi <code>&lt;ref&gt;</code> ƣajrimiçoz; jodkardho biduni mūhtavo bojad nom doşta boşand',
	'cite_error_ref_too_many_keys' => 'Barcasbi  <code>&lt;ref&gt;</code> ƣajrimiçoz; nomhoi ƣajrimiçoz jo beş az andoza',
	'cite_error_ref_no_input' => 'Barcasbi  <code>&lt;ref&gt;</code> ƣajrimiçoz; jodkardhoi biduni nom bojad mūhtavo doşta boşand',
	'cite_error_references_invalid_parameters' => 'Barcasbi <code>&lt;references&gt;</code> ƣajrimiçoz; istifoda az parametr miçoz ast, az  <code>&lt;references /&gt;</code> istifoda kuned',
	'cite_error_references_invalid_parameters_group' => 'Barcasbi <code>&lt;references&gt;</code> nomū\'tabar;
parametri "gurūh" tanho içozaşuda ast.
Barcasbi <code>&lt;references /&gt;</code> jo <code>&lt;references group="..." /&gt;</code> -ro istifoda bared',
	'cite_error_references_no_backlink_label' => 'Barcasbhoi pajvand ba intiho rasid, mavoridi çadidro dar pajjom  <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki> istifoda kuned',
	'cite_error_references_no_text' => 'Barcasbi  <code>&lt;ref&gt;</code> ƣajrimiçoz; matne baroi jodkardho bo nomi <code>$1</code> vorid naşudaast',
);

/** Thai (ไทย)
 * @author Passawuth
 */
$messages['th'] = array(
	'cite-desc' => 'ใส่ <nowiki><ref[ name=id]></nowiki> และ <nowiki><references /></nowiki> สำหรับการอ้างอิง',
	'cite_croak' => 'แหล่งอ้างอิงเสีย; $1: $2',
	'cite_error' => 'อ้างอิงผิดพลาด: $1',
);

/** Turkmen (Türkmençe)
 * @author Hanberke
 */
$messages['tk'] = array(
	'cite_croak' => 'Sita ýitirildi; $1: $2',
	'cite_error_key_str_invalid' => 'Içerki säwlik;
nädogry $str we/ýa-da $key.
Bu asla bolmaly däl.',
	'cite_error_stack_invalid_input' => 'Içerki säwlik;
nädogry stek açary.
Bu asla bolmaly däl.',
	'cite_error' => 'Sitirleme säwligi: $1',
	'cite_error_ref_numeric_key' => 'Nädogry <code>&lt;ref&gt;</code> tegi;
at ýönekeý bir bitin san bolup bilmeýär. Düşündirişli at ulanyň',
	'cite_error_ref_no_key' => 'Nädogry <code>&lt;ref&gt;</code> tegi;
mazmunsyz refleriň ady bolmaly',
	'cite_error_ref_too_many_keys' => 'Nädogry <code>&lt;ref&gt;</code> tegi;
nädogry atlar, mes. aşa köp',
	'cite_error_ref_no_input' => 'Nädogry <code>&lt;ref&gt;</code> tegi;
atsyz refleriň mazmuny bolmalydyr',
	'cite_error_references_invalid_parameters' => 'Nädogry <code>&lt;ref&gt;</code> tegi;
hiç hili parametre rugsat berilmeýär.
<code>&lt;references /&gt;</code> ulanyň',
	'cite_error_references_no_text' => 'Nädogry <code>&lt;ref&gt;</code> tegi;
<code>$1</code> atly refler üçin tekst görkezilmändir',
	'cite_error_included_ref' => '&lt;ref&gt; tegi üçin &lt;/ref&gt; ýapylyşy kem',
	'cite_error_refs_without_references' => '<code>&lt;ref&gt;</code> tegleri bar, emma <code>&lt;references/&gt;</code> tegi tapylmady',
	'cite_error_group_refs_without_references' => '"$1" atly topar üçin <code>&lt;ref&gt;</code> tegleri bar, emma degişli code>&lt;references group="$1"/&gt;</code> tegi tapylmady',
	'cite_error_references_group_mismatch' => '<code>&lt;references&gt;</code>-daky <code>&lt;ref&gt;</code> teginiň çaknyşýan "$1" topar aýratynlygy bar.',
	'cite_error_references_missing_group' => '<code>&lt;references&gt;</code>-da kesgitlenen <code>&lt;ref&gt;</code> teginiň öňki tekstde ýok "$1" topar aýratynlygy bar.',
	'cite_error_references_missing_key' => '<code>&lt;references&gt;</code>-da kesgitlenen "$1" atly <code>&lt;ref&gt;</code> tegi öňki tekstde ulanylmaýar.',
	'cite_error_references_no_key' => '<code>&lt;references&gt;</code>-da kesgitlenen <code>&lt;ref&gt;</code> teginiň hiç hili at aýratynlygy ýok.',
	'cite_error_empty_references_define' => '<code>&lt;references&gt;</code>-da kesgitlenen "$1" atly <code>&lt;ref&gt;</code> tegiň mazmuny ýok.',
);

/** Tagalog (Tagalog)
 * @author AnakngAraw
 */
$messages['tl'] = array(
	'cite-desc' => 'Nagdaragdag ng mga tatak na <nowiki><ref[ name=id]></nowiki> at <nowiki><references/></nowiki>, para sa mga pagtukoy',
	'cite_croak' => 'Nawalan ng buhay ang pagtukoy; $1: $2',
	'cite_error_key_str_invalid' => 'Panloob na kamalian;
hindi tanggap na $str at/o $key.
Hindi ito dapat mangyari.',
	'cite_error_stack_invalid_input' => 'Panloob na kamalian;
hindi tanggap na susi ng salansan.
Hindi ito dapat mangyari.',
	'cite_error' => 'Kamalian sa pagtukoy: $1',
	'cite_error_ref_numeric_key' => "Hindi tanggap ang tatak na <code>&lt;ref&gt;</code>;
hindi maaaring isang payak na buumbilang (''integer'') ang pangalan.  Gumamit ng isang mapaglarawang pamagat",
	'cite_error_ref_no_key' => "Hindi tanggap ang tatak na <code>&lt;ref&gt;</code>;
kinakailangan may isang pangalan ang mga sangguniang (''ref'') walang nilalaman",
	'cite_error_ref_too_many_keys' => 'Hindi tanggap ang tatak na <code>&lt;ref&gt;</code>;
hindi tanggap na mga pangalan, ang ibig sabihin ay napakarami',
	'cite_error_ref_no_input' => "Hindi tanggap ang tatak na <code>&lt;ref&gt;</code>;
kinakailangang may nilalaman ang mga sangguniang (''ref'') walang pangalan",
	'cite_error_references_invalid_parameters' => 'Hindi tanggap na <code>&lt;references&gt;</code>;
walang pinapahintulutang mga parametro.
Gamitin ang <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Hindi tanggap ang tatak na <code>&lt;references&gt;</code>;
Pinapahintulutan lamang ang parametrong "pangkat" (\'\'group\'\').
Gamitin ang <code>&lt;references /&gt;</code>, o <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Naubusan ng pasadyang mga tatak na pantukoy ng panlikod na kawing.
Dagdagan pa ng pakahulugan sa loob ng mensaheng <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_no_link_label_group' => 'Naubusan ng mga tatak ng pasadyang kawing para sa pangkat na "$1".
Magbigay ng marami pang kahulugan sa loob ng mensaheng <nowiki>[[MediaWiki:$2]]</nowiki>.',
	'cite_error_references_no_text' => "Hindi tanggap ang tatak na <code>&lt;ref&gt;</code>;
walang tekstong ibinigay para sa mga sangguniang (''ref'') pinangalanang <code>$1</code>",
	'cite_error_included_ref' => 'Naawawala ang pansarang &lt;/ref&gt; na para sa tatak na &lt;ref&gt;',
	'cite_error_refs_without_references' => 'Umiiral na ang mga tatak na <code>&lt;ref&gt;</code>, subalit walang natagpuang tatak na <code>&lt;references/&gt;</code>',
	'cite_error_group_refs_without_references' => 'Umiiral na ang tatak na <code>&lt;ref&gt;</code> para sa pangkat na pinangalanang "$1", subalit walang natagpuang katumbas na tatak na <code>&lt;references group="$1"/&gt;</code>',
	'cite_error_references_group_mismatch' => 'May hindi nagbabanggaang katangiang pampangkat na "$1" ang <code>&lt;ref&gt;</code> tatak na nasa <code>&lt;references&gt;</code>.',
	'cite_error_references_missing_group' => 'Ang tatak na <code>&lt;ref&gt;</code> na binigyang kahulugan sa <code>&lt;references&gt;</code> ay may katangiang pampangkat na "$1" na hindi lumilitawa sa naunang teksto.',
	'cite_error_references_missing_key' => 'Ang tatak na <code>&lt;ref&gt;</code> na may pangalang "$1" na binigyang kahulugan sa <code>&lt;references&gt;</code> ay hindi ginamit sa naunang teksto.',
	'cite_error_references_no_key' => 'Ang tatak na <code>&lt;ref&gt;</code> na binigyang kahulugan sa <code>&lt;references&gt;</code> ay walang katangiang pampangalan.',
	'cite_error_empty_references_define' => 'Ang tatak na <code>&lt;ref&gt;</code> na binigyang kahulugan sa <code>&lt;references&gt;</code> na may pangalang "$1" ay walang nilalaman.',
);

/** Turkish (Türkçe)
 * @author Joseph
 * @author Runningfridgesrule
 */
$messages['tr'] = array(
	'cite-desc' => 'Alıntılar için, <nowiki><ref[ name=id]></nowiki> ve <nowiki><references/></nowiki> etiketlerini ekler',
	'cite_croak' => 'Alıntı kaybedildi; $1: $2',
	'cite_error_key_str_invalid' => 'Dahili hata;
geçersiz $str ve/ya da $key.
Bu asla olmamalı.',
	'cite_error_stack_invalid_input' => 'Dahili hata;
geçersiz stack anahtarı.
Bu asla olmamalı.',
	'cite_error' => 'Kaynak hatası $1',
	'cite_error_ref_numeric_key' => 'Geçersiz <code>&lt;ref&gt;</code> etiketi;
isim basit bir tamsayı olamaz. Tanımlayıcı bir başlık kullanın',
	'cite_error_ref_no_key' => 'Geçersiz <code>&lt;ref&gt;</code> etiketi;
içeriksiz reflerin bir ismi olmalı',
	'cite_error_ref_too_many_keys' => 'Geçersiz <code>&lt;ref&gt;</code> etiketi;
geçersiz isimler, ör. çok fazla',
	'cite_error_ref_no_input' => 'Geçersiz <code>&lt;ref&gt;</code> etiketi;
isimsiz reflerin içeriği olmalı',
	'cite_error_references_invalid_parameters' => 'Geçersiz <code>&lt;references&gt;</code> etiketi;
parametrelere izin verilmiyor.
<code>&lt;references /&gt;</code> kullanın',
	'cite_error_references_invalid_parameters_group' => 'Geçersiz <code>&lt;references&gt;</code> etiketi;
sadece "group" parametresine izin verilir.
<code>&lt;references /&gt;</code>, ya da <code>&lt;references group="..." /&gt;</code> kullanın',
	'cite_error_references_no_backlink_label' => 'Özel geribağlantı etiketleri kalmadı.
<nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki> mesajında daha fazla tanımlayın',
	'cite_error_no_link_label_group' => '"$1" grubu için özel bağlantı etiketleri bitti.
<nowiki>[[MediaWiki:$2]]</nowiki> mesajında daha fazla tanımlayın.',
	'cite_error_references_no_text' => 'Geçersiz <code>&lt;ref&gt;</code> etiketi;
<code>$1</code> isimli refler için metin temin edilmemiş',
	'cite_error_included_ref' => '&lt;ref&gt; etiketi için &lt;/ref&gt; kapanışı eksik',
	'cite_error_refs_without_references' => '<code>&lt;ref&gt;</code> etiketleri var, ama <code>&lt;references/&gt;</code> etiketi bulunamadı',
	'cite_error_group_refs_without_references' => '"$1" isimli grup için <code>&lt;ref&gt;</code> etiketleri mevcut, ancak karşılık gelen <code>&lt;references group="$1"/&gt;</code> etiketi bulunamadı',
	'cite_error_references_group_mismatch' => '<code>&lt;references&gt;</code>\'daki <code>&lt;ref&gt;</code> etiketinin çelişen "$1" grup özniteliği var.',
	'cite_error_references_missing_group' => '<code>&lt;references&gt;</code>\'da tanımlanan <code>&lt;ref&gt;</code> etiketinin önceki metinde olmayan "$1" grup özniteliği var.',
	'cite_error_references_missing_key' => '<code>&lt;references&gt;</code>\'da tanımlanan "$1" adındaki <code>&lt;ref&gt;</code> etiketi önceki metinde kullanılmıyor.',
	'cite_error_references_no_key' => "<code>&lt;references&gt;</code>'da tanımlanan <code>&lt;ref&gt;</code> etiketinin hiçbir ad özniteliği yok.",
	'cite_error_empty_references_define' => '<code>&lt;references&gt;</code>\'da tanımlanan "$1" adlı <code>&lt;ref&gt;</code> etiketinin içeriği yok.',
);

/** Ukrainian (Українська)
 * @author AS
 * @author Ahonc
 * @author NickK
 * @author Prima klasy4na
 * @author Ytsukeng Fyvaprol
 */
$messages['uk'] = array(
	'cite-desc' => 'Додає теги <nowiki><ref[ name=id]></nowiki> і <nowiki><references/></nowiki> для виносок',
	'cite_croak' => 'Цитата померла; $1: $2',
	'cite_error_key_str_invalid' => 'Внутрішня помилка:
неправильний $str і/або $key.',
	'cite_error_stack_invalid_input' => 'Внутрішня помилка: неправильний ключ стека.',
	'cite_error' => 'Помилка цитування: $1',
	'cite_error_ref_numeric_key' => 'Неправильний виклик <code>&lt;ref&gt;</code>:
назва не може містити тільки цифри.',
	'cite_error_ref_no_key' => 'Неправильний виклик <code>&lt;ref&gt;</code>:
порожній тег <code>ref</code> повинен мати параметр name.',
	'cite_error_ref_too_many_keys' => 'Неправильний виклик <code>&lt;ref&gt;</code>:
вказані неправильні значення <code>name</code> або вказано багато прамаетрів',
	'cite_error_ref_no_input' => 'Неправильний виклик <code>&lt;ref&gt;</code>:
тег <code>ref</code> без назви повинен мати вхідні дані',
	'cite_error_references_invalid_parameters' => 'Неправильний тег <code>&lt;references&gt;</code>:
параметри не передбачені. Використовуйте <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Помилковий тег <code>&lt;references&gt;</code>;
можна використовувати тільки параметр «group».
Використовуйте <code>&lt;references /&gt;</code> або <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Недостатньо символів для зворотних гіперпосилань.
Потрібно розширити системну змінну <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_no_link_label_group' => 'Закінчилися позначки користувальницьких посилань для групи "$1".
Визначте додаткові в повідомленні <nowiki>[[MediaWiki:$2]]</nowiki>.',
	'cite_error_references_no_text' => 'Неправильний виклик <code>&lt;ref&gt;</code>:
для виносок <code>$1</code> не вказаний текст',
	'cite_error_included_ref' => 'Відсутній тег &lt;/ref&gt; за наявності тега &lt;ref&gt;',
	'cite_error_refs_without_references' => "<span style='color: red'>Для існуючого тегу <code>&lt;ref&gt;</code> не знайдено відповідного тегу <code>&lt;references/&gt;</code></span>",
	'cite_error_group_refs_without_references' => 'Для існуючих тегів <code>&lt;ref&gt;</code> групи під назвою "$1" не знайдено відповідного тегу <code>&lt;references group="$1"/&gt;</code>',
	'cite_error_references_group_mismatch' => 'Тег <code>&lt;ref&gt;</code> в <code>&lt;references&gt;</code> має конфліктуючий атрибут групи "$1".',
	'cite_error_references_missing_group' => 'Тег <code>&lt;ref&gt;</code>, заданий в <code>&lt;references&gt;</code>, має атрибут групи "$1", який не фігурує в попередньому тексті.',
	'cite_error_references_missing_key' => 'Тег <code>&lt;ref&gt;</code> з назвою "$1", визначений у <code>&lt;references&gt;</code>, не використовується в попередньому тексті.',
	'cite_error_references_no_key' => 'Тег <code>&lt;ref&gt;</code>, визначений у <code>&lt;references&gt;</code>, не має атрибута назви.',
	'cite_error_empty_references_define' => 'Тег <code>&lt;ref&gt;</code>, визначений у <code>&lt;references&gt;</code>, з назвою "$1" не має змісту.',
	'cite_references_link_one' => "<li id=\"\$1\">'''[[#\$2|↑]]''' \$3</li>",
	'cite_references_link_many_format_backlink_labels' => 'а б в г д е ж и к л м н п р с т у ф х ц ш щ ю я аа аб ав аг ад ае аж аи ак ал ам ан ап ар ас ат ау аф ах ац аш ащ аю ая ба бб бв бг бд бе бж би бк бл бм бн бп бр бс бт бу бф бх бц бш бщ бю бя ва вб вв вг вд ве вж ви вк вл вм вн вп вр вс вт ву вф вх вц вш вщ вю вя га гб гв гг гд ге гж ги гк гл гм гн гп гр гс гт гу гф гх гц гш гщ гю гя да дб дв дг дд де дж ди дк дл дм дн дп др дс дт ду дф дх дц дш дщ дю дя еа еб ев ег ед ее еж еи ек ел ем ен еп ер ес ет еу еф ех ец еш ещ ею ея жа жб жв жг жд же жж жи жк жл жм жн жп жр жс жт жу жф жх жц жш жщ жю жя иа иб ив иг ид ие иж ии ик ил им ин ип ир ис ит иу иф их иц иш ищ ию ия ка кб кв кг кд ке кж ки кк кл км кн кп кр кс кт ку кф кх кц кш кщ кю кя ла лб лв лг лд ле лж ли лк лл лм лн лп лр лс лт лу лф лх лц лш лщ лю ля ма мб мв мг мд ме мж ми мк мл мм мн мп мр мс мт му мф мх мц мш мщ мю мя на нб нв нг нд не нж ни нк нл нм нн нп нр нс нт ну нф нх нц нш нщ ню ня па пб пв пг пд пе пж пи пк пл пм пн пп пр пс пт пу пф пх пц пш пщ пю пя ра рб рв рг рд ре рж ри рк рл рм рн рп рр рс рт ру рф рх рц рш рщ рю ря са сб св сг сд се сж си ск сл см сн сп ср сс ст су сф сх сц сш сщ сю ся та тб тв тг тд те тж ти тк тл тм тн тп тр тс тт ту тф тх тц тш тщ тю тя уа уб ув уг уд уе уж уи ук ул ум ун уп ур ус ут уу уф ух уц уш ущ ую уя фа фб фв фг фд фе фж фи фк фл фм фн фп фр фс фт фу фф фх фц фш фщ фю фя ха хб хв хг хд хе хж хи хк хл хм хн хп хр хс хт ху хф хх хц хш хщ хю хя ца цб цв цг цд це цж ци цк цл цм цн цп цр цс цт цу цф цх цц цш цщ цю ця ша шб шв шг шд ше шж ши шк шл шм шн шп шр шс шт шу шф шх шц шш шщ шю шя ща щб щв щг щд ще щж щи щк щл щм щн щп щр щс щт щу щф щх щц щш щщ щю щя юа юб юв юг юд юе юж юи юк юл юм юн юп юр юс ют юу юф юх юц юш ющ юю юя яа яб яв яг яд яе яж яи як ял ям ян яп яр яс ят яу яф ях яц яш ящ яю яя',
);

/** Vèneto (Vèneto)
 * @author Candalua
 */
$messages['vec'] = array(
	'cite-desc' => 'Zonta i tag <nowiki><ref[ name=id]></nowiki> e <nowiki><references/></nowiki> par gestir le citazion',
	'cite_croak' => 'Eror ne la citazion: $1: $2',
	'cite_error_key_str_invalid' => 'Eror interno: $str e/o $key sbaglià. Sta roba qua no la dovarìa mai capitar.',
	'cite_error_stack_invalid_input' => 'Eror interno;
ciave de stack sbaglià.
Sta roba no la dovarìa mai capitar.',
	'cite_error' => 'Eror ne la funsion Cite $1',
	'cite_error_ref_numeric_key' => "Eror ne l'uso del marcator <code>&lt;ref&gt;</code>: el nome no'l pode mìa èssar un nùmaro intiero. Dòpara un titolo esteso",
	'cite_error_ref_no_key' => "Eror ne l'uso del marcator <code>&lt;ref&gt;</code>: i ref vodi no i pol no verghe un nome",
	'cite_error_ref_too_many_keys' => "Eror ne l'uso del marcator <code>&lt;ref&gt;</code>: nomi mìa validi (ad es. nùmaro massa elevà)",
	'cite_error_ref_no_input' => "Eror ne l'uso del marcator <code>&lt;ref&gt;</code>: i ref che no gà un nome no i pol mìa èssar vodi",
	'cite_error_references_invalid_parameters' => "Eror ne l'uso del marcator <code>&lt;references&gt;</code>: parametri mìa consentìi, dòpara el marcator <code>&lt;references /&gt;</code>",
	'cite_error_references_invalid_parameters_group' => 'Tag <code>&lt;references&gt;</code> mìa valido;
solo el parametro "group" el xe permesso.
Dòpara <code>&lt;references /&gt;</code>, o <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Etichete de rimando personalizàe esaurìe, auménteghen el nùmaro nel messagio <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_no_link_label_group' => 'Etichete esaurìe par colegamenti personalizà del grupo "$1", aumentarne el numaro nel messajo <nowiki>[[MediaWiki:$2]]</nowiki>',
	'cite_error_references_no_text' => 'Marcator <code>&lt;ref&gt;</code> mìa valido; no xe stà indicà nissun testo par el marcator <code>$1</code>',
	'cite_error_included_ref' => '&lt;/ref&gt; de chiusura mancante par el marcador &lt;ref&gt;',
	'cite_error_refs_without_references' => 'Ghe xe un tag <code>&lt;ref&gt;</code>, ma no xe stà catà nissun tag <code>&lt;references/&gt;</code>',
	'cite_error_group_refs_without_references' => 'Ghe xe un tag <code>&lt;ref&gt;</code> par un grupo che se ciama "$1", ma no xe stà catà nissun tag <code>&lt;references group="$1"/&gt;</code> che corisponda.',
	'cite_error_references_group_mismatch' => 'El tag <code>&lt;ref&gt;</code> in <code>&lt;references&gt;</code> el gà l\'atributo de grupo "$1" in conflito.',
	'cite_error_references_missing_group' => 'El tag <code>&lt;ref&gt;</code> definìo in <code>&lt;references&gt;</code> el gà un atributo de grupo "$1" che no conpare mia nel testo precedente.',
	'cite_error_references_missing_key' => 'El tag <code>&lt;ref&gt;</code> con nome "$1" definìo in <code>&lt;references&gt;</code> no\'l xe doparà nel testo precedente.',
	'cite_error_references_no_key' => "El tag <code>&lt;ref&gt;</code> definìo in <code>&lt;references&gt;</code> no'l gà un atributo nome.",
	'cite_error_empty_references_define' => 'El tag <code>&lt;ref&gt;</code> definìo in <code>&lt;references&gt;</code> con nome "$1" no\'l gà nissun contenuto.',
);

/** Veps (Vepsan kel')
 * @author Игорь Бродский
 */
$messages['vep'] = array(
	'cite_error' => 'Citiruindan petuz: $1',
);

/** Vietnamese (Tiếng Việt)
 * @author Minh Nguyen
 * @author Vinhtantran
 */
$messages['vi'] = array(
	'cite-desc' => 'Thêm các thẻ <nowiki><ref[ name=id]></nowiki> và <nowiki><references/></nowiki> để ghi chú thích hoặc nguồn tham khảo',
	'cite_croak' => 'Chú thích bị hỏng; $1: $2',
	'cite_error_key_str_invalid' => 'Lỗi nội bộ; $str và/hoặc $key không hợp lệ. Điều này không bao giờ nên xảy ra.',
	'cite_error_stack_invalid_input' => 'Lỗi nội bộ; khóa xác định chồng bị sai.  Đáng ra không bao giờ xảy ra điều này.',
	'cite_error' => 'Lỗi chú thích: $1',
	'cite_error_ref_numeric_key' => 'Thẻ <code>&lt;ref&gt;</code> sai; tên không thể chỉ là số nguyên, hãy dùng tựa đề có tính miêu tả',
	'cite_error_ref_no_key' => 'Thẻ <code>&lt;ref&gt;</code> sai; thẻ ref không có nội dung thì phải có tên',
	'cite_error_ref_too_many_keys' => 'Thẻ <code>&lt;ref&gt;</code> sai; thông số tên sai, như, nhiều thông số tên quá',
	'cite_error_ref_no_input' => 'Mã <code>&lt;ref&gt;</code> sai; thẻ ref không có tên thì phải có nội dung',
	'cite_error_references_invalid_parameters' => 'Thẻ <code>&lt;references&gt;</code> sai; không được có thông số, hãy dùng <code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => 'Thẻ <code>&lt;references&gt;</code> không hợp lệ;
chỉ cho phép tham số “group”.
Hãy dùng <code>&lt;references /&gt;</code>, hoặc <code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => 'Đã dùng hết nhãn tham khảo chung.
Hãy định nghĩa thêm ở thông báo <nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>',
	'cite_error_no_link_label_group' => 'Thiếu nhãn liên kết tùy biến cho nhóm “$1”. Hãy định rõ thêm nhãn trong thông báo <nowiki>[[MediaWiki:$2]]</nowiki>.',
	'cite_error_references_no_text' => 'Thẻ <code>&lt;ref&gt;</code> sai; không có nội dung trong thẻ ref có tên <code>$1</code>',
	'cite_error_included_ref' => 'Không có &lt;/ref&gt; để đóng thẻ &lt;ref&gt;',
	'cite_error_refs_without_references' => 'Tồn tại thẻ <code>&lt;ref&gt;</code>, nhưng không tìm thấy thẻ <code>&lt;references/&gt;</code>',
	'cite_error_group_refs_without_references' => 'Tồn tại thẻ <code>&lt;ref&gt;</code> với tên nhóm “$1”, nhưng không tìm thấy thẻ <code>&lt;references group="$1"/&gt;</code> tương ứng',
	'cite_error_references_group_mismatch' => 'Thẻ <code>&lt;ref&gt;</code> trong <code>&lt;references&gt;</code> có thuộc tính nhóm mâu thuẫn “$1”.',
	'cite_error_references_missing_group' => 'Thẻ <code>&lt;ref&gt;</code> được định nghĩa trong <code>&lt;references&gt;</code> có thuộc tính nhóm  “$1” không thấy xuất hiện trong văn bản phía trên.',
	'cite_error_references_missing_key' => 'Thẻ <code>&lt;ref&gt;</code> có tên  “$1” được định nghĩa trong <code>&lt;references&gt;</code> không được đoạn văn bản trên sử dụng.',
	'cite_error_references_no_key' => ' Thẻ <code>&lt;ref&gt;</code> được định nghĩa trong <code>&lt;references&gt;</code> không có thuộc tính name.',
	'cite_error_empty_references_define' => 'Thẻ <code>&lt;ref&gt;</code> được định nghĩa trong <code>&lt;references&gt;</code> có tên “$1” không có nội dung.',
);

/** Volapük (Volapük)
 * @author Smeira
 */
$messages['vo'] = array(
	'cite_croak' => 'Saitot dädik; $1: $2',
	'cite_error_key_str_invalid' => 'Pöl ninik: $str e/u $key no lonöföl(s). Atos no sötonöv jenön.',
	'cite_error_stack_invalid_input' => 'Pöl ninik; kumakik no lonöföl. Atos neai sötonöv jenön.',
	'cite_error' => 'Saitamapöl: $1',
	'cite_error_ref_numeric_key' => 'Nem ela <code>&lt;ref&gt;</code> no lonöföl. Nem no kanon binädön te me numats; gebolös bepenami.',
	'cite_error_ref_no_key' => 'Geb no lonöföl ela <code>&lt;ref&gt;</code>: els ref nen ninäd mutons labön nemi',
	'cite_error_ref_too_many_keys' => 'El <code>&lt;ref&gt;</code> no lonöfon: labon nemis no lonöfikis, a. s. tumödikis',
	'cite_error_ref_no_input' => 'El <code>&lt;ref&gt;</code> no lonöfon: els ref nen nem mutons labön ninädi',
	'cite_error_references_invalid_parameters' => 'El <code>&lt;references&gt;</code> no lonöfon: paramets no padälons. Gebolös eli <code>&lt;references /&gt;</code>',
	'cite_error_references_no_text' => 'El <code>&lt;ref&gt;</code> no lonöfon: vödem nonik pegivon eles refs labü nem: <code>$1</code>',
);

/** Yiddish (ייִדיש)
 * @author פוילישער
 */
$messages['yi'] = array(
	'cite-desc' => 'לייגט צו <nowiki><ref[ name=id]></nowiki> און <nowiki><references/></nowiki> טאַגן, פֿאר ציטירונגען (אין הערות)',
	'cite_croak' => 'טעות אין ציטירונג; $1: $2',
	'cite_error' => 'ציטירן גרײַז: $1',
	'cite_error_ref_numeric_key' => 'גרײַזיגער <code>&lt;ref&gt;</code> טאַג;
נאמען טאר נישט זײַן קיין פשוטער נומער. ניצט א באשרײַבדיק קעפל',
	'cite_error_ref_no_key' => 'אומגילדיגער <code>&lt;ref&gt;</code> טאַג;
א רעפֿערענץ אָן תוכן מוז האבן א נאמען',
	'cite_error_ref_too_many_keys' => 'אומגילטיגער <code>&lt;ref&gt;</code> טאַג;
אומגילטיגע נעמען, צ.ב. צו פֿיל',
	'cite_error_ref_no_input' => 'אומגילטיגער <code>&lt;ref&gt;</code> טאַג;
א רעפֿערענץ אָן א נאמען דארף האבן תוכן',
	'cite_error_references_invalid_parameters' => 'אומגילטיגער <code>&lt;references&gt;</code> טאַג;
קיין פאראמעטערס נישט ערלויבט. ניצט <code>&lt;references /&gt;</code>',
	'cite_error_references_no_text' => 'אומגילטיגער <code>&lt;ref&gt;</code> טאַג;
קיין טעקסט נישט געשריבן פֿאַר רעפֿערענצן מיטן נאמן <code>$1</code>',
	'cite_error_included_ref' => 'פֿעלט א שליסנדיגער &lt;/ref&gt; פֿאַר &lt;ref&gt; טאַג',
	'cite_error_refs_without_references' => "ס'זענען דא <code>&lt;ref&gt;</code> טאַגן, אבער מ'האט נישט געטראפֿן קיין <code>&lt;references/&gt;</code> טאַג.",
	'cite_error_group_refs_without_references' => 'ס\'זענען דא <code>&lt;ref&gt;</code> טאַגן פֿאַר א גרופע וואס הייסט "$1", אבער מ\'האט נישט געטראפֿן קיין אַנטקעגענעם  <code>&lt;references group="$1"/&gt;</code> טאַג.',
	'cite_error_references_missing_key' => '<code>&lt;ref&gt;</code> טאַג מיטן נאָמען "$1" דעפֿינירט אין<code>&lt;references&gt;</code> נישט געניצט אין פֿריערדיקן טעקסט.',
	'cite_error_references_no_key' => '<code>&lt;ref&gt;</code> טאַג דעפֿינירט אין <code>&lt;references&gt;</code> האט נישט קיין name אַטריבוט.',
	'cite_error_empty_references_define' => '<code>&lt;ref&gt;</code> טאַג דעפֿינירט אין <code>&lt;references&gt;</code> מיט נאָמען "$1" האט נישט קיין אינהאַלט.',
);

/** Cantonese (粵語) */
$messages['yue'] = array(
	'cite-desc' => '加 <nowiki><ref[ name=id]></nowiki> 同 <nowiki><references/></nowiki> 標籤用響引用度',
	'cite_croak' => '引用阻塞咗; $1: $2',
	'cite_error_key_str_invalid' => '內部錯誤; 無效嘅 $str',
	'cite_error_stack_invalid_input' => '內部錯誤; 無效嘅堆疊匙',
	'cite_error' => '引用錯誤 $1',
	'cite_error_ref_numeric_key' => '無效嘅呼叫; 需要一個非整數嘅匙',
	'cite_error_ref_no_key' => '無效嘅呼叫; 未指定匙',
	'cite_error_ref_too_many_keys' => '無效嘅呼叫; 無效嘅匙, 例如: 太多或者指定咗一個錯咗嘅匙',
	'cite_error_ref_no_input' => '無效嘅呼叫; 未指定輸入',
	'cite_error_references_invalid_parameters' => '無效嘅參數; 唔需要有嘢',
	'cite_error_references_invalid_parameters_group' => '無效嘅<code>&lt;references&gt;</code>標籤；
只容許 "group" 參數。
用<code>&lt;references /&gt;</code>，或<code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => '用晒啲自定返回標籤, 響 <nowiki>[[MediaWiki:Cite_references_link_many_format_backlink_labels]]</nowiki> 信息再整多啲',
	'cite_error_references_no_text' => '無效嘅<code>&lt;ref&gt;</code>標籤；
無文字提供於名為<code>$1</code>嘅參照',
);

/** Simplified Chinese (‪中文(简体)‬)
 * @author Gaoxuewei
 * @author Liangent
 * @author Philip
 */
$messages['zh-hans'] = array(
	'cite-desc' => '增加用于引用的<nowiki><ref[ name=id]></nowiki>和<nowiki><references/></nowiki>标签',
	'cite_croak' => '引用失效；$1：$2',
	'cite_error_key_str_invalid' => '内部错误；不应出现的非法$str和／或$key。',
	'cite_error_stack_invalid_input' => '内部错误；不应出现的非法堆栈键值。',
	'cite_error' => '引用错误：$1',
	'cite_error_ref_numeric_key' => '无效<code>&lt;ref&gt;</code>标签；name属性不能是单一的数字，请使用可辨识的标题',
	'cite_error_ref_no_key' => '无效<code>&lt;ref&gt;</code>标签；未填内容的引用必须填写name属性',
	'cite_error_ref_too_many_keys' => '无效<code>&lt;ref&gt;</code>标签；name属性非法，可能是内容过长',
	'cite_error_ref_no_input' => '无效<code>&lt;ref&gt;</code>标签；未填name属性的引用必须填写内容',
	'cite_error_references_invalid_parameters' => '无效<code>&lt;references&gt;</code>标签；不允许填写参数，请使用<code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => '无效<code>&lt;references&gt;</code>标签；只允许填写“group”参数，请使用<code>&lt;references /&gt;</code>或<code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => '自定义回链标签耗尽。请在<nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>中定义更多的标签',
	'cite_error_references_no_text' => '无效<code>&lt;ref&gt;</code>标签；未为name属性为<code>$1</code>的引用提供文字',
	'cite_error_included_ref' => '没有找到与&lt;/ref&gt;对应的&lt;ref&gt;标签',
	'cite_error_refs_without_references' => '<code>&lt;ref&gt;</code>标签存在，但没有找到<code>&lt;references/&gt;</code>标签',
	'cite_error_group_refs_without_references' => '组名为“$1”的<code>&lt;ref&gt;</code>标签存在，但没有找到相应的<code>&lt;references group="$1"/&gt;</code>标签',
	'cite_error_references_missing_key' => '在<code>&lt;references&gt;</code>中以“$1”名字定义的<code>&lt;ref&gt;</code>标签没有在先前的文字中使用。',
);

/** Traditional Chinese (‪中文(繁體)‬)
 * @author Liangent
 * @author Philip
 */
$messages['zh-hant'] = array(
	'cite-desc' => '增加用於引用的<nowiki><ref[ name=id]></nowiki>和<nowiki><references/></nowiki>標簽',
	'cite_croak' => '引用失效；$1：$2',
	'cite_error_key_str_invalid' => '內部錯誤；不應出現的非法$str和／或$key。',
	'cite_error_stack_invalid_input' => '內部錯誤；不應出現的非法堆疊鍵值。',
	'cite_error' => '引用錯誤：$1',
	'cite_error_ref_numeric_key' => '無效<code>&lt;ref&gt;</code>標簽；name屬性不能是單一的數字，請使用可辨識的標題',
	'cite_error_ref_no_key' => '無效<code>&lt;ref&gt;</code>標簽；未填內容的引用必須填寫name屬性',
	'cite_error_ref_too_many_keys' => '無效<code>&lt;ref&gt;</code>標簽；name屬性非法，可能是內容過長',
	'cite_error_ref_no_input' => '無效<code>&lt;ref&gt;</code>標簽；未填name屬性的引用必須填寫內容',
	'cite_error_references_invalid_parameters' => '無效<code>&lt;references&gt;</code>標簽；不允许填寫參量，請使用<code>&lt;references /&gt;</code>',
	'cite_error_references_invalid_parameters_group' => '無效<code>&lt;references&gt;</code>標簽；只允许填寫「group」參量，請使用<code>&lt;references /&gt;</code>或<code>&lt;references group="..." /&gt;</code>',
	'cite_error_references_no_backlink_label' => '自定義回連標簽耗盡。請在<nowiki>[[MediaWiki:Cite references link many format backlink labels]]</nowiki>中定義更多的標簽',
	'cite_error_references_no_text' => '無效<code>&lt;ref&gt;</code>標簽；未為name屬性為<code>$1</code>的引用提供文字',
	'cite_error_included_ref' => '沒有找到與&lt;/ref&gt;對應的&lt;ref&gt;標簽',
	'cite_error_refs_without_references' => '<code>&lt;ref&gt;</code>標簽存在，但沒有找到<code>&lt;references/&gt;</code>標簽',
	'cite_error_group_refs_without_references' => '組名為「$1」的<code>&lt;ref&gt;</code>標簽存在，但沒有找到相應的<code>&lt;references group="$1"/&gt;</code>標簽',
	'cite_error_references_missing_key' => '在<code>&lt;references&gt;</code>中以“$1”名字定義的<code>&lt;ref&gt;</code>標籤沒有在先前的文字中使用。',
);


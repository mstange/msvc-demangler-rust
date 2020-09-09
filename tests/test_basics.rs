extern crate msvc_demangler;

use msvc_demangler::{demangle, DemangleFlags};

fn expect_with_flags(input: &str, reference: &str, flags: u32) {
    let demangled = demangle(input, ::DemangleFlags::from_bits(flags).unwrap());
    let reference = reference.to_owned();
    if let Ok(demangled) = demangled {
        assert_eq!(demangled, reference);
    } else {
        panic!("{:?} != {:?}", demangled, Ok::<_, ()>(reference));
    }
}

// For cases where undname demangles differently/better than we do.
fn expect_failure(input: &str, reference: &str) {
    let demangled = demangle(input, ::DemangleFlags::COMPLETE).unwrap();
    let reference = reference.to_owned();
    assert_ne!(demangled, reference);
}
// std::basic_filebuf<char,struct std::char_traits<char> >::basic_filebuf<char,struct std::char_traits<char> >
// std::basic_filebuf<char,struct std::char_traits<char> >::"operator ctor"
// "operator ctor" = ?0

#[test]
fn other_tests() {
    let expect = |input, reference| {
        expect_with_flags(input, reference, 0x0);
    };

    expect("?f@@YAHQBH@Z", "int __cdecl f(int const * const)");
    expect("?f@@YA_WQB_W@Z", "wchar_t __cdecl f(wchar_t const * const)");
    expect(
        "?f@@YA_UQB_U@Z",
        "char32_t __cdecl f(char32_t const * const)",
    );
    expect(
        "?f@@YA_SQB_S@Z",
        "char16_t __cdecl f(char16_t const * const)",
    );
    expect(
        "?g@@YAHQAY0EA@$$CBH@Z",
        "int __cdecl g(int const (* const)[64])",
    );
    expect(
        "??0Klass@std@@AEAA@AEBV01@@Z",
        "private: __cdecl std::Klass::Klass(class std::Klass const &)",
    );
    expect("??0?$Klass@V?$Mass@_N@@@std@@QEAA@AEBV01@@Z",
            "public: __cdecl std::Klass<class Mass<bool> >::Klass<class Mass<bool> >(class std::Klass<class Mass<bool> > const &)");
    expect(
        "??$load@M@UnsharedOps@js@@SAMV?$SharedMem@PAM@@@Z",
        "public: static float __cdecl js::UnsharedOps::load<float>(class SharedMem<float *>)",
    );

    expect("?cached@?1??GetLong@BinaryPath@mozilla@@SA?AW4nsresult@@QA_W@Z@4_NA",
            "bool `public: static enum nsresult __cdecl mozilla::BinaryPath::GetLong(wchar_t * const)\'::`2\'::cached");
    expect(
        "??0?$A@_K@B@@QAE@$$QAV01@@Z",
        "public: __thiscall B::A<uint64_t>::A<uint64_t>(class B::A<uint64_t> &&)",
    );
    expect("??_7nsI@@6B@", "const nsI::`vftable\'");
    expect("??_7W@?A@@6B@", "const `anonymous namespace'::W::`vftable'");
    expect(
        "??_7?$RunnableMethodImpl@PEAVLazyIdleThread@mozilla@@P812@EAAXXZ$0A@$0A@$$V@detail@mozilla@@6BnsIRunnable@@@",
        "const mozilla::detail::RunnableMethodImpl<class mozilla::LazyIdleThread *,void (__cdecl mozilla::LazyIdleThread::*)(void),0,0>::`vftable\'{for `nsIRunnable\'}",
    );
    expect_failure(
        "??_7?$RunnableMethodImpl@PEAVLazyIdleThread@mozilla@@P812@EAAXXZ$0A@$0A@$$V@detail@mozilla@@6BnsIRunnable@@@",
        "const mozilla::detail::RunnableMethodImpl<class mozilla::LazyIdleThread * __ptr64,void (__cdecl mozilla::LazyIdleThread::*)(void) __ptr64,0,0>::`vftable\'{for `nsIRunnable\'}",
    );
    expect(
        "??1?$ns@$$CBVtxXP@@@@QAE@XZ",
        "public: __thiscall ns<class txXP const>::~ns<class txXP const>(void)",
    );
    /* XXX: undname prints void (__thiscall*)(void *) for the parameter type. */
    expect(
        "??_I@YGXPAXIIP6EX0@Z@Z",
        "void __stdcall `vector destructor iterator'(void *,unsigned int,unsigned int,void (__thiscall *)(void *))",
    );
    expect(
        "??_GnsWindowsShellService@@EAEPAXI@Z",
        "private: virtual void * __thiscall nsWindowsShellService::`scalar deleting destructor'(unsigned int)",
    );
    expect(
        "??1?$nsAutoPtr@$$CBVtxXPathNode@@@@QAE@XZ",
        "public: __thiscall nsAutoPtr<class txXPathNode const>::~nsAutoPtr<class txXPathNode const>(void)",
    );
    expect(
        "??_EPrintfTarget@mozilla@@MAEPAXI@Z",
        "protected: virtual void * __thiscall mozilla::PrintfTarget::`vector deleting destructor'(unsigned int)",
    );
    expect(
        "??_GDynamicFrameEventFilter@?A0xcdaa5fa8@@AAEPAXI@Z",
        "private: void * __thiscall `anonymous namespace'::DynamicFrameEventFilter::`scalar deleting destructor\'(unsigned int)",
    );
    /* XXX: undname tacks on `adjustor{16}` to the name. */
    expect(
        "?Release@ContentSignatureVerifier@@WBA@AGKXZ",
        "[thunk]: public: virtual unsigned long __stdcall ContentSignatureVerifier::Release(void)",
    );
    expect(
        "??$new_@VWatchpointMap@js@@$$V@?$MallocProvider@UZone@JS@@@js@@QAEPAVWatchpointMap@1@XZ",
        "public: class js::WatchpointMap * __thiscall js::MallocProvider<struct JS::Zone>::new_<class js::WatchpointMap>(void)",
    );
    expect(
        "??$templ_fun_with_ty_pack@$$V@@YAXXZ",
        "void __cdecl templ_fun_with_ty_pack<>(void)",
    );
    expect(
        "??4?$RefPtr@VnsRange@@@@QAEAAV0@$$T@Z",
        "public: class RefPtr<class nsRange> & __thiscall RefPtr<class nsRange>::operator=(std::nullptr_t)",
    );
    expect(
        "??1?$function@$$A6AXXZ@std@@QAE@XZ",
        "public: __thiscall std::function<void __cdecl (void)>::~function<void __cdecl (void)>(void)",
    );
    expect_failure(
        "??1?$function@$$A6AXXZ@std@@QAE@XZ",
        "public: __thiscall std::function<void __cdecl(void)>::~function<void __cdecl(void)>(void)",
    );
    expect(
        "??B?$function@$$A6AXXZ@std@@QBE_NXZ",
        "public: bool __thiscall std::function<void __cdecl (void)>::operator bool(void) const",
    );
    expect_failure(
        "??B?$function@$$A6AXXZ@std@@QBE_NXZ",
        "public: __thiscall std::function<void __cdecl(void)>::operator bool(void) const",
    );
    expect(
        "??$?RA6AXXZ$$V@SkOnce@@QAEXA6AXXZ@Z",
        "public: void __thiscall SkOnce::operator()<void (__cdecl &)(void)>(void (__cdecl &)(void))",
    );
    expect_failure(
        "??$?RA6AXXZ$$V@SkOnce@@QAEXA6AXXZ@Z",
        "public: void __thiscall SkOnce::operator()<void (__cdecl&)(void)>(void (__cdecl&)(void))",
    );
    expect(
        "?foo@A@PR19361@@QIHAEXXZ",
        "public: void __thiscall PR19361::A::foo(void) __restrict &&",
    );
    expect_failure(
        "?foo@A@PR19361@@QIHAEXXZ",
        "public: void __thiscall PR19361::A::foo(void) __restrict&& ",
    );
    expect(
        "??$GenericCreateConstructor@$1?construct@SetObject@js@@CA_NPEAUJSContext@@IPEATValue@JS@@@Z$0A@$0A@$0A@@js@@YAPEAVJSObject@@PEAUJSContext@@W4JSProtoKey@@@Z",
        "class JSObject * __cdecl js::GenericCreateConstructor<bool (__cdecl js::SetObject::construct::*)(struct JSContext *,unsigned int,union JS::Value *),0,0,0>(struct JSContext *,enum JSProtoKey)",
    );
    expect_failure(
        "??$GenericCreateConstructor@$1?construct@SetObject@js@@CA_NPEAUJSContext@@IPEATValue@JS@@@Z$0A@$0A@$0A@@js@@YAPEAVJSObject@@PEAUJSContext@@W4JSProtoKey@@@Z",
        "class JSObject * __ptr64 __cdecl js::GenericCreateConstructor<&private: static bool (__cdecl js::SetObject::construct::*)(struct JSContext * __ptr64,unsigned int,union JS::Value * __ptr64),0,0,0>(struct JSContext * __ptr64,enum JSProtoKey)",
    );
    expect(
        "??$emplace_hint@AEBUpiecewise_construct_t@std@@V?$tuple@AEBH@2@V?$tuple@$$V@2@@?$_Tree@V?$_Tmap_traits@HUPayload@RtpUtility@webrtc@@U?$less@H@std@@V?$allocator@U?$pair@$$CBHUPayload@RtpUtility@webrtc@@@std@@@5@$0A@@std@@@std@@QEAA?AV?$_Tree_iterator@V?$_Tree_val@U?$_Tree_simple_types@U?$pair@$$CBHUPayload@RtpUtility@webrtc@@@std@@@std@@@std@@@1@V?$_Tree_const_iterator@V?$_Tree_val@U?$_Tree_simple_types@U?$pair@$$CBHUPayload@RtpUtility@webrtc@@@std@@@std@@@std@@@1@AEBUpiecewise_construct_t@1@$$QEAV?$tuple@AEBH@1@$$QEAV?$tuple@$$V@1@@Z",
        "public: class std::_Tree_iterator<class std::_Tree_val<struct std::_Tree_simple_types<struct std::pair<int const,struct webrtc::RtpUtility::Payload> > > > __cdecl std::_Tree<class std::_Tmap_traits<int,struct webrtc::RtpUtility::Payload,struct std::less<int>,class std::allocator<struct std::pair<int const,struct webrtc::RtpUtility::Payload> >,0> >::emplace_hint<struct std::piecewise_construct_t const &,class std::tuple<int const &>,class std::tuple<> >(class std::_Tree_const_iterator<class std::_Tree_val<struct std::_Tree_simple_types<struct std::pair<int const,struct webrtc::RtpUtility::Payload> > > >,struct std::piecewise_construct_t const &,class std::tuple<int const &> &&,class std::tuple<> &&)",
    );
    expect(
        "?_OptionsStorage@?1??__local_stdio_scanf_options@@9@9",
        "`__local_stdio_scanf_options'::`2'::_OptionsStorage",
    );
    expect(
        "??_9nsDocument@@$BDMI@AE",
        "[thunk]: __thiscall nsDocument::`vcall'{968,{flat}}",
    );
    expect(
        "??_R0?AUCollationCacheEntry@icu_61@@@8",
        "struct icu_61::CollationCacheEntry::`RTTI Type Descriptor\'",
    );
    expect(
        "??_R0?AV?$KxTree@V?$KxSpe@DI@@I@@@8",
        "class KxTree<class KxSpe<char,unsigned int>,unsigned int>::`RTTI Type Descriptor'",
    );
    expect("??_R2A@@8", "A::`RTTI Base Class Array'");
    expect("??_R3UO@i@@8", "i::UO::`RTTI Class Hierarchy Descriptor'");
    expect(
        "??_R1A@?0A@EA@U@i@@8",
        "i::U::`RTTI Base Class Descriptor at (0,-1,0,64)'",
    );
    expect(
        "?getFactory@SkImageShader@@UBEP6A?AV?$sk_sp@VSkFlattenable@@@@AAVSkReadBuffer@@@ZXZ",
        "public: virtual class sk_sp<class SkFlattenable> (__cdecl * __thiscall SkImageShader::getFactory(void) const)(class SkReadBuffer &)"
    );

    expect(
        "?Present1@?QIDXGISwapChain4@@CDXGISwapChain@@UAGJIIPBUDXGI_PRESENT_PARAMETERS@@@Z", 
        "public: virtual long __stdcall CDXGISwapChain::[IDXGISwapChain4]::Present1(unsigned int,unsigned int,struct DXGI_PRESENT_PARAMETERS const *)");

    // An MD5 mangled name is "valid" but output as-is
    expect(
        "??@8ba8d245c9eca390356129098dbe9f73@",
        "??@8ba8d245c9eca390356129098dbe9f73@",
    );
}

#[test]
fn test_strings() {
    let expect = |input, reference| {
        expect_with_flags(input, reference, 0x0);
    };

    // Test symbols extracted from clang's test/CodeGenCXX/mangle-ms-string-literals.cpp.
    // Even though we don't print the encoded strings, these tests
    // exhaustively cover all the cases we'll run into.

    // Single-byte characters.
    expect("??_C@_01CNACBAHC@?$PP?$AA@", "`string'");
    expect("??_C@_01DEBJCBDD@?$PO?$AA@", "`string'");
    expect("??_C@_01BPDEHCPA@?$PN?$AA@", "`string'");
    expect("??_C@_01GCPEDLB@?$PM?$AA@", "`string'");
    expect("??_C@_01EJGONFHG@?$PL?$AA@", "`string'");
    expect("??_C@_01FAHFOEDH@?z?$AA@", "`string'");
    expect("??_C@_01HLFILHPE@?y?$AA@", "`string'");
    expect("??_C@_01GCEDIGLF@?x?$AA@", "`string'");
    expect("??_C@_01OFNLJKHK@?w?$AA@", "`string'");
    expect("??_C@_01PMMAKLDL@?v?$AA@", "`string'");
    expect("??_C@_01NHONPIPI@?u?$AA@", "`string'");
    expect("??_C@_01MOPGMJLJ@?t?$AA@", "`string'");
    expect("??_C@_01IBLHFPHO@?s?$AA@", "`string'");
    expect("??_C@_01JIKMGODP@?r?$AA@", "`string'");
    expect("??_C@_01LDIBDNPM@?q?$AA@", "`string'");
    expect("??_C@_01KKJKAMLN@?p?$AA@", "`string'");
    expect("??_C@_01GHMAACCD@?o?$AA@", "`string'");
    expect("??_C@_01HONLDDGC@?n?$AA@", "`string'");
    expect("??_C@_01FFPGGAKB@?m?$AA@", "`string'");
    expect("??_C@_01EMONFBOA@?l?$AA@", "`string'");
    expect("??_C@_01DKMMHCH@?k?$AA@", "`string'");
    expect("??_C@_01BKLHPGGG@?j?$AA@", "`string'");
    expect("??_C@_01DBJKKFKF@?i?$AA@", "`string'");
    expect("??_C@_01CIIBJEOE@?h?$AA@", "`string'");
    expect("??_C@_01KPBJIICL@?g?$AA@", "`string'");
    expect("??_C@_01LGACLJGK@?f?$AA@", "`string'");
    expect("??_C@_01JNCPOKKJ@?e?$AA@", "`string'");
    expect("??_C@_01IEDENLOI@?d?$AA@", "`string'");
    expect("??_C@_01MLHFENCP@?c?$AA@", "`string'");
    expect("??_C@_01NCGOHMGO@?b?$AA@", "`string'");
    expect("??_C@_01PJEDCPKN@?a?$AA@", "`string'");
    expect("??_C@_01OAFIBOOM@?$OA?$AA@", "`string'");
    expect("??_C@_01LIIGDENA@?$NP?$AA@", "`string'");
    expect("??_C@_01KBJNAFJB@?$NO?$AA@", "`string'");
    expect("??_C@_01IKLAFGFC@?$NN?$AA@", "`string'");
    expect("??_C@_01JDKLGHBD@?$NM?$AA@", "`string'");
    expect("??_C@_01NMOKPBNE@?$NL?$AA@", "`string'");
    expect("??_C@_01MFPBMAJF@?Z?$AA@", "`string'");
    expect("??_C@_01OONMJDFG@?Y?$AA@", "`string'");
    expect("??_C@_01PHMHKCBH@?X?$AA@", "`string'");
    expect("??_C@_01HAFPLONI@?W?$AA@", "`string'");
    expect("??_C@_01GJEEIPJJ@?V?$AA@", "`string'");
    expect("??_C@_01ECGJNMFK@?U?$AA@", "`string'");
    expect("??_C@_01FLHCONBL@?T?$AA@", "`string'");
    expect("??_C@_01BEDDHLNM@?S?$AA@", "`string'");
    expect("??_C@_01NCIEKJN@?R?$AA@", "`string'");
    expect("??_C@_01CGAFBJFO@?Q?$AA@", "`string'");
    expect("??_C@_01DPBOCIBP@?P?$AA@", "`string'");
    expect("??_C@_01PCEECGIB@?O?$AA@", "`string'");
    expect("??_C@_01OLFPBHMA@?N?$AA@", "`string'");
    expect("??_C@_01MAHCEEAD@?M?$AA@", "`string'");
    expect("??_C@_01NJGJHFEC@?L?$AA@", "`string'");
    expect("??_C@_01JGCIODIF@?K?$AA@", "`string'");
    expect("??_C@_01IPDDNCME@?J?$AA@", "`string'");
    expect("??_C@_01KEBOIBAH@?I?$AA@", "`string'");
    expect("??_C@_01LNAFLAEG@?H?$AA@", "`string'");
    expect("??_C@_01DKJNKMIJ@?G?$AA@", "`string'");
    expect("??_C@_01CDIGJNMI@?F?$AA@", "`string'");
    expect("??_C@_01IKLMOAL@?E?$AA@", "`string'");
    expect("??_C@_01BBLAPPEK@?D?$AA@", "`string'");
    expect("??_C@_01FOPBGJIN@?C?$AA@", "`string'");
    expect("??_C@_01EHOKFIMM@?B?$AA@", "`string'");
    expect("??_C@_01GMMHALAP@?A?$AA@", "`string'");
    expect("??_C@_01HFNMDKEO@?$MA?$AA@", "`string'");
    expect("??_C@_01NNHLFPHH@?$LP?$AA@", "`string'");
    expect("??_C@_01MEGAGODG@?$LO?$AA@", "`string'");
    expect("??_C@_01OPENDNPF@?$LN?$AA@", "`string'");
    expect("??_C@_01PGFGAMLE@?$LM?$AA@", "`string'");
    expect("??_C@_01LJBHJKHD@?$LL?$AA@", "`string'");
    expect("??_C@_01KAAMKLDC@?$LK?$AA@", "`string'");
    expect("??_C@_01ILCBPIPB@?$LJ?$AA@", "`string'");
    expect("??_C@_01JCDKMJLA@?$LI?$AA@", "`string'");
    expect("??_C@_01BFKCNFHP@?$LH?$AA@", "`string'");
    expect("??_C@_01MLJOEDO@?$LG?$AA@", "`string'");
    expect("??_C@_01CHJELHPN@?$LF?$AA@", "`string'");
    expect("??_C@_01DOIPIGLM@?$LE?$AA@", "`string'");
    expect("??_C@_01HBMOBAHL@?$LD?$AA@", "`string'");
    expect("??_C@_01GINFCBDK@?$LC?$AA@", "`string'");
    expect("??_C@_01EDPIHCPJ@?$LB?$AA@", "`string'");
    expect("??_C@_01FKODEDLI@?$LA?$AA@", "`string'");
    expect("??_C@_01JHLJENCG@?$KP?$AA@", "`string'");
    expect("??_C@_01IOKCHMGH@?$KO?$AA@", "`string'");
    expect("??_C@_01KFIPCPKE@?$KN?$AA@", "`string'");
    expect("??_C@_01LMJEBOOF@?$KM?$AA@", "`string'");
    expect("??_C@_01PDNFIICC@?$KL?$AA@", "`string'");
    expect("??_C@_01OKMOLJGD@?$KK?$AA@", "`string'");
    expect("??_C@_01MBODOKKA@?$KJ?$AA@", "`string'");
    expect("??_C@_01NIPINLOB@?$KI?$AA@", "`string'");
    expect("??_C@_01FPGAMHCO@?$KH?$AA@", "`string'");
    expect("??_C@_01EGHLPGGP@?$KG?$AA@", "`string'");
    expect("??_C@_01GNFGKFKM@?$KF?$AA@", "`string'");
    expect("??_C@_01HEENJEON@?$KE?$AA@", "`string'");
    expect("??_C@_01DLAMACCK@?$KD?$AA@", "`string'");
    expect("??_C@_01CCBHDDGL@?$KC?$AA@", "`string'");
    expect("??_C@_01JDKGAKI@?$KB?$AA@", "`string'");
    expect("??_C@_01BACBFBOJ@?$KA?$AA@", "`string'");
    expect("??_C@_01EIPPHLNF@?$JP?$AA@", "`string'");
    expect("??_C@_01FBOEEKJE@?$JO?$AA@", "`string'");
    expect("??_C@_01HKMJBJFH@?$JN?$AA@", "`string'");
    expect("??_C@_01GDNCCIBG@?$JM?$AA@", "`string'");
    expect("??_C@_01CMJDLONB@?$JL?$AA@", "`string'");
    expect("??_C@_01DFIIIPJA@?$JK?$AA@", "`string'");
    expect("??_C@_01BOKFNMFD@?$JJ?$AA@", "`string'");
    expect("??_C@_01HLOONBC@?$JI?$AA@", "`string'");
    expect("??_C@_01IACGPBNN@?$JH?$AA@", "`string'");
    expect("??_C@_01JJDNMAJM@?$JG?$AA@", "`string'");
    expect("??_C@_01LCBAJDFP@?$JF?$AA@", "`string'");
    expect("??_C@_01KLALKCBO@?$JE?$AA@", "`string'");
    expect("??_C@_01OEEKDENJ@?$JD?$AA@", "`string'");
    expect("??_C@_01PNFBAFJI@?$JC?$AA@", "`string'");
    expect("??_C@_01NGHMFGFL@?$JB?$AA@", "`string'");
    expect("??_C@_01MPGHGHBK@?$JA?$AA@", "`string'");
    expect("??_C@_01CDNGJIE@?$IP?$AA@", "`string'");
    expect("??_C@_01BLCGFIMF@?$IO?$AA@", "`string'");
    expect("??_C@_01DAALALAG@?$IN?$AA@", "`string'");
    expect("??_C@_01CJBADKEH@?$IM?$AA@", "`string'");
    expect("??_C@_01GGFBKMIA@?$IL?$AA@", "`string'");
    expect("??_C@_01HPEKJNMB@?$IK?$AA@", "`string'");
    expect("??_C@_01FEGHMOAC@?$IJ?$AA@", "`string'");
    expect("??_C@_01ENHMPPED@?$II?$AA@", "`string'");
    expect("??_C@_01MKOEODIM@?$IH?$AA@", "`string'");
    expect("??_C@_01NDPPNCMN@?$IG?$AA@", "`string'");
    expect("??_C@_01PINCIBAO@?$IF?$AA@", "`string'");
    expect("??_C@_01OBMJLAEP@?$IE?$AA@", "`string'");
    expect("??_C@_01KOIICGII@?$ID?$AA@", "`string'");
    expect("??_C@_01LHJDBHMJ@?$IC?$AA@", "`string'");
    expect("??_C@_01JMLOEEAK@?$IB?$AA@", "`string'");
    expect("??_C@_01IFKFHFEL@?$IA?$AA@", "`string'");
    expect("??_C@_01BGIBIIDJ@?$HP?$AA@", "`string'");
    expect("??_C@_01PJKLJHI@?$HO?$AA@", "`string'");
    expect("??_C@_01CELHOKLL@?$HN?$AA@", "`string'");
    expect("??_C@_01DNKMNLPK@?$HM?$AA@", "`string'");
    expect("??_C@_01HCONENDN@?$HL?$AA@", "`string'");
    expect("??_C@_01GLPGHMHM@z?$AA@", "`string'");
    expect("??_C@_01EANLCPLP@y?$AA@", "`string'");
    expect("??_C@_01FJMABOPO@x?$AA@", "`string'");
    expect("??_C@_01NOFIACDB@w?$AA@", "`string'");
    expect("??_C@_01MHEDDDHA@v?$AA@", "`string'");
    expect("??_C@_01OMGOGALD@u?$AA@", "`string'");
    expect("??_C@_01PFHFFBPC@t?$AA@", "`string'");
    expect("??_C@_01LKDEMHDF@s?$AA@", "`string'");
    expect("??_C@_01KDCPPGHE@r?$AA@", "`string'");
    expect("??_C@_01IIACKFLH@q?$AA@", "`string'");
    expect("??_C@_01JBBJJEPG@p?$AA@", "`string'");
    expect("??_C@_01FMEDJKGI@o?$AA@", "`string'");
    expect("??_C@_01EFFIKLCJ@n?$AA@", "`string'");
    expect("??_C@_01GOHFPIOK@m?$AA@", "`string'");
    expect("??_C@_01HHGOMJKL@l?$AA@", "`string'");
    expect("??_C@_01DICPFPGM@k?$AA@", "`string'");
    expect("??_C@_01CBDEGOCN@j?$AA@", "`string'");
    expect("??_C@_01KBJDNOO@i?$AA@", "`string'");
    expect("??_C@_01BDACAMKP@h?$AA@", "`string'");
    expect("??_C@_01JEJKBAGA@g?$AA@", "`string'");
    expect("??_C@_01INIBCBCB@f?$AA@", "`string'");
    expect("??_C@_01KGKMHCOC@e?$AA@", "`string'");
    expect("??_C@_01LPLHEDKD@d?$AA@", "`string'");
    expect("??_C@_01PAPGNFGE@c?$AA@", "`string'");
    expect("??_C@_01OJONOECF@b?$AA@", "`string'");
    expect("??_C@_01MCMALHOG@a?$AA@", "`string'");
    expect("??_C@_01NLNLIGKH@?$GA?$AA@", "`string'");
    expect("??_C@_01IDAFKMJL@_?$AA@", "`string'");
    expect("??_C@_01JKBOJNNK@?$FO?$AA@", "`string'");
    expect("??_C@_01LBDDMOBJ@?$FN?$AA@", "`string'");
    expect("??_C@_01KICIPPFI@?2?$AA@", "`string'");
    expect("??_C@_01OHGJGJJP@?$FL?$AA@", "`string'");
    expect("??_C@_01POHCFINO@Z?$AA@", "`string'");
    expect("??_C@_01NFFPALBN@Y?$AA@", "`string'");
    expect("??_C@_01MMEEDKFM@X?$AA@", "`string'");
    expect("??_C@_01ELNMCGJD@W?$AA@", "`string'");
    expect("??_C@_01FCMHBHNC@V?$AA@", "`string'");
    expect("??_C@_01HJOKEEBB@U?$AA@", "`string'");
    expect("??_C@_01GAPBHFFA@T?$AA@", "`string'");
    expect("??_C@_01CPLAODJH@S?$AA@", "`string'");
    expect("??_C@_01DGKLNCNG@R?$AA@", "`string'");
    expect("??_C@_01BNIGIBBF@Q?$AA@", "`string'");
    expect("??_C@_01EJNLAFE@P?$AA@", "`string'");
    expect("??_C@_01MJMHLOMK@O?$AA@", "`string'");
    expect("??_C@_01NANMIPIL@N?$AA@", "`string'");
    expect("??_C@_01PLPBNMEI@M?$AA@", "`string'");
    expect("??_C@_01OCOKONAJ@L?$AA@", "`string'");
    expect("??_C@_01KNKLHLMO@K?$AA@", "`string'");
    expect("??_C@_01LELAEKIP@J?$AA@", "`string'");
    expect("??_C@_01JPJNBJEM@I?$AA@", "`string'");
    expect("??_C@_01IGIGCIAN@H?$AA@", "`string'");
    expect("??_C@_01BBODEMC@G?$AA@", "`string'");
    expect("??_C@_01BIAFAFID@F?$AA@", "`string'");
    expect("??_C@_01DDCIFGEA@E?$AA@", "`string'");
    expect("??_C@_01CKDDGHAB@D?$AA@", "`string'");
    expect("??_C@_01GFHCPBMG@C?$AA@", "`string'");
    expect("??_C@_01HMGJMAIH@B?$AA@", "`string'");
    expect("??_C@_01FHEEJDEE@A?$AA@", "`string'");
    expect("??_C@_01EOFPKCAF@?$EA?$AA@", "`string'");
    expect("??_C@_01OGPIMHDM@?$DP?$AA@", "`string'");
    expect("??_C@_01PPODPGHN@?$DO?$AA@", "`string'");
    expect("??_C@_01NEMOKFLO@?$DN?$AA@", "`string'");
    expect("??_C@_01MNNFJEPP@?$DM?$AA@", "`string'");
    expect("??_C@_01ICJEACDI@?$DL?$AA@", "`string'");
    expect("??_C@_01JLIPDDHJ@?3?$AA@", "`string'");
    expect("??_C@_01LAKCGALK@9?$AA@", "`string'");
    expect("??_C@_01KJLJFBPL@8?$AA@", "`string'");
    expect("??_C@_01COCBENDE@7?$AA@", "`string'");
    expect("??_C@_01DHDKHMHF@6?$AA@", "`string'");
    expect("??_C@_01BMBHCPLG@5?$AA@", "`string'");
    expect("??_C@_01FAMBOPH@4?$AA@", "`string'");
    expect("??_C@_01EKENIIDA@3?$AA@", "`string'");
    expect("??_C@_01FDFGLJHB@2?$AA@", "`string'");
    expect("??_C@_01HIHLOKLC@1?$AA@", "`string'");
    expect("??_C@_01GBGANLPD@0?$AA@", "`string'");
    expect("??_C@_01KMDKNFGN@?1?$AA@", "`string'");
    expect("??_C@_01LFCBOECM@?4?$AA@", "`string'");
    expect("??_C@_01JOAMLHOP@?9?$AA@", "`string'");
    expect("??_C@_01IHBHIGKO@?0?$AA@", "`string'");
    expect("??_C@_01MIFGBAGJ@?$CL?$AA@", "`string'");
    expect("??_C@_01NBENCBCI@?$CK?$AA@", "`string'");
    expect("??_C@_01PKGAHCOL@?$CJ?$AA@", "`string'");
    expect("??_C@_01ODHLEDKK@?$CI?$AA@", "`string'");
    expect("??_C@_01GEODFPGF@?8?$AA@", "`string'");
    expect("??_C@_01HNPIGOCE@?$CG?$AA@", "`string'");
    expect("??_C@_01FGNFDNOH@?$CF?$AA@", "`string'");
    expect("??_C@_01EPMOAMKG@$?$AA@", "`string'");
    expect("??_C@_01IPJKGB@?$CD?$AA@", "`string'");
    expect("??_C@_01BJJEKLCA@?$CC?$AA@", "`string'");
    expect("??_C@_01DCLJPIOD@?$CB?$AA@", "`string'");
    expect("??_C@_01CLKCMJKC@?5?$AA@", "`string'");
    expect("??_C@_01HDHMODJO@?$BP?$AA@", "`string'");
    expect("??_C@_01GKGHNCNP@?$BO?$AA@", "`string'");
    expect("??_C@_01EBEKIBBM@?$BN?$AA@", "`string'");
    expect("??_C@_01FIFBLAFN@?$BM?$AA@", "`string'");
    expect("??_C@_01BHBACGJK@?$BL?$AA@", "`string'");
    expect("??_C@_01OALBHNL@?$BK?$AA@", "`string'");
    expect("??_C@_01CFCGEEBI@?$BJ?$AA@", "`string'");
    expect("??_C@_01DMDNHFFJ@?$BI?$AA@", "`string'");
    expect("??_C@_01LLKFGJJG@?$BH?$AA@", "`string'");
    expect("??_C@_01KCLOFINH@?$BG?$AA@", "`string'");
    expect("??_C@_01IJJDALBE@?$BF?$AA@", "`string'");
    expect("??_C@_01JAIIDKFF@?$BE?$AA@", "`string'");
    expect("??_C@_01NPMJKMJC@?$BD?$AA@", "`string'");
    expect("??_C@_01MGNCJNND@?$BC?$AA@", "`string'");
    expect("??_C@_01ONPPMOBA@?$BB?$AA@", "`string'");
    expect("??_C@_01PEOEPPFB@?$BA?$AA@", "`string'");
    expect("??_C@_01DJLOPBMP@?$AP?$AA@", "`string'");
    expect("??_C@_01CAKFMAIO@?$AO?$AA@", "`string'");
    expect("??_C@_01LIIJDEN@?$AN?$AA@", "`string'");
    expect("??_C@_01BCJDKCAM@?$AM?$AA@", "`string'");
    expect("??_C@_01FNNCDEML@?$AL?$AA@", "`string'");
    expect("??_C@_01EEMJAFIK@?6?$AA@", "`string'");
    expect("??_C@_01GPOEFGEJ@?7?$AA@", "`string'");
    expect("??_C@_01HGPPGHAI@?$AI?$AA@", "`string'");
    expect("??_C@_01PBGHHLMH@?$AH?$AA@", "`string'");
    expect("??_C@_01OIHMEKIG@?$AG?$AA@", "`string'");
    expect("??_C@_01MDFBBJEF@?$AF?$AA@", "`string'");
    expect("??_C@_01NKEKCIAE@?$AE?$AA@", "`string'");
    expect("??_C@_01JFALLOMD@?$AD?$AA@", "`string'");
    expect("??_C@_01IMBAIPIC@?$AC?$AA@", "`string'");
    expect("??_C@_01KHDNNMEB@?$AB?$AA@", "`string'");
    expect("??_C@_01LOCGONAA@?$AA?$AA@", "`string'");

    // Wide characters.
    expect("??_C@_13KDLDGPGJ@?$AA?7?$AA?$AA@", "`string'");
    expect("??_C@_13LBAGMAIH@?$AA?6?$AA?$AA@", "`string'");
    expect("??_C@_13JLKKHOC@?$AA?$AL?$AA?$AA@", "`string'");
    expect("??_C@_13HOIJIPNN@?$AA?5?$AA?$AA@", "`string'");
    expect("??_C@_13MGDFOILI@?$AA?$CB?$AA?$AA@", "`string'");
    expect("??_C@_13NEIAEHFG@?$AA?$CC?$AA?$AA@", "`string'");
    expect("??_C@_13GMDMCADD@?$AA?$CD?$AA?$AA@", "`string'");
    expect("??_C@_13PBOLBIIK@?$AA$?$AA?$AA@", "`string'");
    expect("??_C@_13EJFHHPOP@?$AA?$CF?$AA?$AA@", "`string'");
    expect("??_C@_13FLOCNAAB@?$AA?$CG?$AA?$AA@", "`string'");
    expect("??_C@_13ODFOLHGE@?$AA?8?$AA?$AA@", "`string'");
    expect("??_C@_13LLDNKHDC@?$AA?$CI?$AA?$AA@", "`string'");
    expect("??_C@_13DIBMAFH@?$AA?$CJ?$AA?$AA@", "`string'");
    expect("??_C@_13BBDEGPLJ@?$AA?$CK?$AA?$AA@", "`string'");
    expect("??_C@_13KJIIAINM@?$AA?$CL?$AA?$AA@", "`string'");
    expect("??_C@_13DEFPDAGF@?$AA?0?$AA?$AA@", "`string'");
    expect("??_C@_13IMODFHAA@?$AA?9?$AA?$AA@", "`string'");
    expect("??_C@_13JOFGPIOO@?$AA?4?$AA?$AA@", "`string'");
    expect("??_C@_13CGOKJPIL@?$AA?1?$AA?$AA@", "`string'");
    expect("??_C@_13COJANIEC@?$AA0?$AA?$AA@", "`string'");
    expect("??_C@_13JGCMLPCH@?$AA1?$AA?$AA@", "`string'");
    expect("??_C@_13IEJJBAMJ@?$AA2?$AA?$AA@", "`string'");
    expect("??_C@_13DMCFHHKM@?$AA3?$AA?$AA@", "`string'");
    expect("??_C@_13KBPCEPBF@?$AA4?$AA?$AA@", "`string'");
    expect("??_C@_13BJEOCIHA@?$AA5?$AA?$AA@", "`string'");
    expect("??_C@_13LPLIHJO@?$AA6?$AA?$AA@", "`string'");
    expect("??_C@_13LDEHOAPL@?$AA7?$AA?$AA@", "`string'");
    expect("??_C@_13OLCEPAKN@?$AA8?$AA?$AA@", "`string'");
    expect("??_C@_13FDJIJHMI@?$AA9?$AA?$AA@", "`string'");
    expect("??_C@_13EBCNDICG@?$AA?3?$AA?$AA@", "`string'");
    expect("??_C@_13PJJBFPED@?$AA?$DL?$AA?$AA@", "`string'");
    expect("??_C@_13GEEGGHPK@?$AA?$DM?$AA?$AA@", "`string'");
    expect("??_C@_13NMPKAAJP@?$AA?$DN?$AA?$AA@", "`string'");
    expect("??_C@_13MOEPKPHB@?$AA?$DO?$AA?$AA@", "`string'");
    expect("??_C@_13HGPDMIBE@?$AA?$DP?$AA?$AA@", "`string'");
    expect("??_C@_13EFKPHINO@?$AA?$EA?$AA?$AA@", "`string'");
    expect("??_C@_13PNBDBPLL@?$AAA?$AA?$AA@", "`string'");
    expect("??_C@_13OPKGLAFF@?$AAB?$AA?$AA@", "`string'");
    expect("??_C@_13FHBKNHDA@?$AAC?$AA?$AA@", "`string'");
    expect("??_C@_13MKMNOPIJ@?$AAD?$AA?$AA@", "`string'");
    expect("??_C@_13HCHBIIOM@?$AAE?$AA?$AA@", "`string'");
    expect("??_C@_13GAMECHAC@?$AAF?$AA?$AA@", "`string'");
    expect("??_C@_13NIHIEAGH@?$AAG?$AA?$AA@", "`string'");
    expect("??_C@_13IABLFADB@?$AAH?$AA?$AA@", "`string'");
    expect("??_C@_13DIKHDHFE@?$AAI?$AA?$AA@", "`string'");
    expect("??_C@_13CKBCJILK@?$AAJ?$AA?$AA@", "`string'");
    expect("??_C@_13JCKOPPNP@?$AAK?$AA?$AA@", "`string'");
    expect("??_C@_13PHJMHGG@?$AAL?$AA?$AA@", "`string'");
    expect("??_C@_13LHMFKAAD@?$AAM?$AA?$AA@", "`string'");
    expect("??_C@_13KFHAAPON@?$AAN?$AA?$AA@", "`string'");
    expect("??_C@_13BNMMGIII@?$AAO?$AA?$AA@", "`string'");
    expect("??_C@_13BFLGCPEB@?$AAP?$AA?$AA@", "`string'");
    expect("??_C@_13KNAKEICE@?$AAQ?$AA?$AA@", "`string'");
    expect("??_C@_13LPLPOHMK@?$AAR?$AA?$AA@", "`string'");
    expect("??_C@_13HADIAKP@?$AAS?$AA?$AA@", "`string'");
    expect("??_C@_13JKNELIBG@?$AAT?$AA?$AA@", "`string'");
    expect("??_C@_13CCGINPHD@?$AAU?$AA?$AA@", "`string'");
    expect("??_C@_13DANNHAJN@?$AAV?$AA?$AA@", "`string'");
    expect("??_C@_13IIGBBHPI@?$AAW?$AA?$AA@", "`string'");
    expect("??_C@_13NAACAHKO@?$AAX?$AA?$AA@", "`string'");
    expect("??_C@_13GILOGAML@?$AAY?$AA?$AA@", "`string'");
    expect("??_C@_13HKALMPCF@?$AAZ?$AA?$AA@", "`string'");
    expect("??_C@_13MCLHKIEA@?$AA?$FL?$AA?$AA@", "`string'");
    expect("??_C@_13FPGAJAPJ@?$AA?2?$AA?$AA@", "`string'");
    expect("??_C@_13OHNMPHJM@?$AA?$FN?$AA?$AA@", "`string'");
    expect("??_C@_13PFGJFIHC@?$AA?$FO?$AA?$AA@", "`string'");
    expect("??_C@_13ENNFDPBH@?$AA_?$AA?$AA@", "`string'");
    expect("??_C@_13OFJNNHOA@?$AA?$GA?$AA?$AA@", "`string'");
    expect("??_C@_13FNCBLAIF@?$AAa?$AA?$AA@", "`string'");
    expect("??_C@_13EPJEBPGL@?$AAb?$AA?$AA@", "`string'");
    expect("??_C@_13PHCIHIAO@?$AAc?$AA?$AA@", "`string'");
    expect("??_C@_13GKPPEALH@?$AAd?$AA?$AA@", "`string'");
    expect("??_C@_13NCEDCHNC@?$AAe?$AA?$AA@", "`string'");
    expect("??_C@_13MAPGIIDM@?$AAf?$AA?$AA@", "`string'");
    expect("??_C@_13HIEKOPFJ@?$AAg?$AA?$AA@", "`string'");
    expect("??_C@_13CACJPPAP@?$AAh?$AA?$AA@", "`string'");
    expect("??_C@_13JIJFJIGK@?$AAi?$AA?$AA@", "`string'");
    expect("??_C@_13IKCADHIE@?$AAj?$AA?$AA@", "`string'");
    expect("??_C@_13DCJMFAOB@?$AAk?$AA?$AA@", "`string'");
    expect("??_C@_13KPELGIFI@?$AAl?$AA?$AA@", "`string'");
    expect("??_C@_13BHPHAPDN@?$AAm?$AA?$AA@", "`string'");
    expect("??_C@_13FECKAND@?$AAn?$AA?$AA@", "`string'");
    expect("??_C@_13LNPOMHLG@?$AAo?$AA?$AA@", "`string'");
    expect("??_C@_13LFIEIAHP@?$AAp?$AA?$AA@", "`string'");
    expect("??_C@_13NDIOHBK@?$AAq?$AA?$AA@", "`string'");
    expect("??_C@_13BPINEIPE@?$AAr?$AA?$AA@", "`string'");
    expect("??_C@_13KHDBCPJB@?$AAs?$AA?$AA@", "`string'");
    expect("??_C@_13DKOGBHCI@?$AAt?$AA?$AA@", "`string'");
    expect("??_C@_13ICFKHAEN@?$AAu?$AA?$AA@", "`string'");
    expect("??_C@_13JAOPNPKD@?$AAv?$AA?$AA@", "`string'");
    expect("??_C@_13CIFDLIMG@?$AAw?$AA?$AA@", "`string'");
    expect("??_C@_13HADAKIJA@?$AAx?$AA?$AA@", "`string'");
    expect("??_C@_13MIIMMPPF@?$AAy?$AA?$AA@", "`string'");
    expect("??_C@_13NKDJGABL@?$AAz?$AA?$AA@", "`string'");
    expect("??_C@_13GCIFAHHO@?$AA?$HL?$AA?$AA@", "`string'");
    expect("??_C@_13PPFCDPMH@?$AA?$HM?$AA?$AA@", "`string'");
    expect("??_C@_13EHOOFIKC@?$AA?$HN?$AA?$AA@", "`string'");
    expect("??_C@_13FFFLPHEM@?$AA?$HO?$AA?$AA@", "`string'");

    // Tests for maximum string length
    expect(
        "??_C@_0CF@LABBIIMO@012345678901234567890123456789AB@",
        "`string'",
    );
    expect("??_C@_1EK@KFPEBLPK@?$AA0?$AA1?$AA2?$AA3?$AA4?$AA5?$AA6?$AA7?$AA8?$AA9?$AA0?$AA1?$AA2?$AA3?$AA4?$AA5?$AA6?$AA7?$AA8?$AA9?$AA0?$AA1?$AA2?$AA3?$AA4?$AA5?$AA6?$AA7?$AA8?$AA9?$AAA?$AAB@", "`string'");
    // Unicode character.
    expect("??_C@_13IIHIAFKH@?W?$PP?$AA?$AA@", "`string'");
    // u8/u/U literal strings.
    expect("??_C@_02PCEFGMJL@hi?$AA@", "`string'");
    expect("??_C@_05OMLEGLOC@h?$AAi?$AA?$AA?$AA@", "`string'");
    expect(
        "??_C@_0M@GFNAJIPG@h?$AA?$AA?$AAi?$AA?$AA?$AA?$AA?$AA?$AA?$AA@",
        "`string'",
    );
}

#[test]
fn upstream_tests() {
    let expect = |input, reference| {
        expect_with_flags(input, reference, 0x0);
    };
    expect("?x@@3HA", "int x");
    expect("?x@@3PEAHEA", "int *x");
    expect("?x@@3PEAPEAHEA", "int * *x");
    expect("?x@@3PEAY02HEA", "int (*x)[3]");
    expect("?x@@3PEAY124HEA", "int (*x)[3][5]");
    expect("?x@@3PEAY02$$CBHEA", "int const (*x)[3]");
    expect("?x@@3PEAEEA", "unsigned char *x");
    expect("?x@@3PEAY1NKM@5HEA", "int (*x)[3500][6]");
    expect("?x@@YAXMH@Z", "void __cdecl x(float,int)");
    expect("?x@@YAXMH@Z", "void __cdecl x(float,int)");
    expect("?x@@3P6AHMNH@ZEA", "int (__cdecl *x)(float,double,int)");
    expect(
        "?x@@3P6AHP6AHM@ZN@ZEA",
        "int (__cdecl *x)(int (__cdecl *)(float),double)",
    );
    expect(
        "?x@@3P6AHP6AHM@Z0@ZEA",
        "int (__cdecl *x)(int (__cdecl *)(float),int (__cdecl *)(float))",
    );

    expect("?x@ns@@3HA", "int ns::x");

    // Microsoft's undname returns "int const * const x" for this symbol.
    // I believe it's their bug.
    expect("?x@@3PEBHEB", "int const *x");

    expect("?x@@3QEAHEB", "int * const x");
    expect("?x@@3QEBHEB", "int const * const x");

    expect("?x@@3AEBHEB", "int const & x");

    expect("?x@@3PEAUty@@EA", "struct ty *x");
    expect("?x@@3PEATty@@EA", "union ty *x");
    expect("?x@@3PEAUty@@EA", "struct ty *x");
    expect("?x@@3PEAW4ty@@EA", "enum ty *x");
    expect("?x@@3PEAVty@@EA", "class ty *x");

    expect("?x@@3PEAV?$tmpl@H@@EA", "class tmpl<int> *x");
    expect("?x@@3PEAU?$tmpl@H@@EA", "struct tmpl<int> *x");
    expect("?x@@3PEAT?$tmpl@H@@EA", "union tmpl<int> *x");
    expect("?instance@@3Vklass@@A", "class klass instance");
    expect(
        "?instance$initializer$@@3P6AXXZEA",
        "void (__cdecl *instance$initializer$)(void)",
    );
    expect("??0klass@@QEAA@XZ", "public: __cdecl klass::klass(void)");
    expect("??1klass@@QEAA@XZ", "public: __cdecl klass::~klass(void)");
    expect(
        "?x@@YAHPEAVklass@@AEAV1@@Z",
        "int __cdecl x(class klass *,class klass &)",
    );
    expect(
        "?x@ns@@3PEAV?$klass@HH@1@EA",
        "class ns::klass<int,int> *ns::x",
    );
    expect(
        "?fn@?$klass@H@ns@@QEBAIXZ",
        "public: unsigned int __cdecl ns::klass<int>::fn(void) const",
    );

    expect(
        "??4klass@@QEAAAEBV0@AEBV0@@Z",
        "public: class klass const & __cdecl klass::operator=(class klass const &)",
    );
    expect(
        "??7klass@@QEAA_NXZ",
        "public: bool __cdecl klass::operator!(void)",
    );
    expect(
        "??8klass@@QEAA_NAEBV0@@Z",
        "public: bool __cdecl klass::operator==(class klass const &)",
    );
    expect(
        "??9klass@@QEAA_NAEBV0@@Z",
        "public: bool __cdecl klass::operator!=(class klass const &)",
    );
    expect(
        "??Aklass@@QEAAH_K@Z",
        "public: int __cdecl klass::operator[](uint64_t)",
    );
    expect(
        "??Cklass@@QEAAHXZ",
        "public: int __cdecl klass::operator->(void)",
    );
    expect(
        "??Dklass@@QEAAHXZ",
        "public: int __cdecl klass::operator*(void)",
    );
    expect(
        "??Eklass@@QEAAHXZ",
        "public: int __cdecl klass::operator++(void)",
    );
    expect(
        "??Eklass@@QEAAHH@Z",
        "public: int __cdecl klass::operator++(int)",
    );
    expect(
        "??Fklass@@QEAAHXZ",
        "public: int __cdecl klass::operator--(void)",
    );
    expect(
        "??Fklass@@QEAAHH@Z",
        "public: int __cdecl klass::operator--(int)",
    );
    expect(
        "??Hklass@@QEAAHH@Z",
        "public: int __cdecl klass::operator+(int)",
    );
    expect(
        "??Gklass@@QEAAHH@Z",
        "public: int __cdecl klass::operator-(int)",
    );
    expect(
        "??Iklass@@QEAAHH@Z",
        "public: int __cdecl klass::operator&(int)",
    );
    expect(
        "??Jklass@@QEAAHH@Z",
        "public: int __cdecl klass::operator->*(int)",
    );
    expect(
        "??Kklass@@QEAAHH@Z",
        "public: int __cdecl klass::operator/(int)",
    );
    expect(
        "??Mklass@@QEAAHH@Z",
        "public: int __cdecl klass::operator<(int)",
    );
    expect(
        "??Nklass@@QEAAHH@Z",
        "public: int __cdecl klass::operator<=(int)",
    );
    expect(
        "??Oklass@@QEAAHH@Z",
        "public: int __cdecl klass::operator>(int)",
    );
    expect(
        "??Pklass@@QEAAHH@Z",
        "public: int __cdecl klass::operator>=(int)",
    );
    expect(
        "??Qklass@@QEAAHH@Z",
        "public: int __cdecl klass::operator,(int)",
    );
    expect(
        "??Rklass@@QEAAHH@Z",
        "public: int __cdecl klass::operator()(int)",
    );
    expect(
        "??Sklass@@QEAAHXZ",
        "public: int __cdecl klass::operator~(void)",
    );
    expect(
        "??Tklass@@QEAAHH@Z",
        "public: int __cdecl klass::operator^(int)",
    );
    expect(
        "??Uklass@@QEAAHH@Z",
        "public: int __cdecl klass::operator|(int)",
    );
    expect(
        "??Vklass@@QEAAHH@Z",
        "public: int __cdecl klass::operator&&(int)",
    );
    expect(
        "??Wklass@@QEAAHH@Z",
        "public: int __cdecl klass::operator||(int)",
    );
    expect(
        "??Xklass@@QEAAHH@Z",
        "public: int __cdecl klass::operator*=(int)",
    );
    expect(
        "??Yklass@@QEAAHH@Z",
        "public: int __cdecl klass::operator+=(int)",
    );
    expect(
        "??Zklass@@QEAAHH@Z",
        "public: int __cdecl klass::operator-=(int)",
    );
    expect(
        "??_0klass@@QEAAHH@Z",
        "public: int __cdecl klass::operator/=(int)",
    );
    expect(
        "??_1klass@@QEAAHH@Z",
        "public: int __cdecl klass::operator%=(int)",
    );
    expect(
        "??_2klass@@QEAAHH@Z",
        "public: int __cdecl klass::operator>>=(int)",
    );
    expect(
        "??_3klass@@QEAAHH@Z",
        "public: int __cdecl klass::operator<<=(int)",
    );
    expect(
        "??_6klass@@QEAAHH@Z",
        "public: int __cdecl klass::operator^=(int)",
    );
    expect(
        "??6@YAAEBVklass@@AEBV0@H@Z",
        "class klass const & __cdecl operator<<(class klass const &,int)",
    );
    expect(
        "??5@YAAEBVklass@@AEBV0@_K@Z",
        "class klass const & __cdecl operator>>(class klass const &,uint64_t)",
    );
    expect(
        "??2@YAPEAX_KAEAVklass@@@Z",
        "void * __cdecl operator new(uint64_t,class klass &)",
    );
    expect(
        "??_U@YAPEAX_KAEAVklass@@@Z",
        "void * __cdecl operator new[](uint64_t,class klass &)",
    );
    expect(
        "??3@YAXPEAXAEAVklass@@@Z",
        "void __cdecl operator delete(void *,class klass &)",
    );
    expect(
        "??_V@YAXPEAXAEAVklass@@@Z",
        "void __cdecl operator delete[](void *,class klass &)",
    );
    expect(
        "?DispatchToCallback@?$I@U?$Y@$S@y@x@@$$V@y@x@@QEAAXV?$C@$$A6AXXZ@base@@@Z",
        "public: void __cdecl x::y::I<struct x::y::Y<> >::DispatchToCallback(class base::C<void __cdecl (void)>)",
    );
    expect(
        "?DispatchToCallback@?$I@U?$Y@$$Z@y@x@@$$V@y@x@@QEAAXV?$C@$$A6AXXZ@base@@@Z",
        "public: void __cdecl x::y::I<struct x::y::Y<> >::DispatchToCallback(class base::C<void __cdecl (void)>)",
    );
    expect(
        "??$func@H$$ZH@@YAHAEBU?$Foo@H@@0@Z",
        "int __cdecl func<int,int>(struct Foo<int> const &,struct Foo<int> const &)",
    );
    expect(
        "??$func@HH$$Z@@YAHAEBU?$Foo@H@@0@Z",
        "int __cdecl func<int,int>(struct Foo<int> const &,struct Foo<int> const &)",
    );
    expect(
        "??$templ_fun_with_pack@$S@@YAXXZ",
        "void __cdecl templ_fun_with_pack<>(void)",
    );
    expect(
        "??$templ_fun_with_ty_pack@$$$V@@YAXXZ",
        "void __cdecl templ_fun_with_ty_pack<>(void)",
    );
    expect(
        "??$templ_fun_with_ty_pack@$$V@@YAXXZ",
        "void __cdecl templ_fun_with_ty_pack<>(void)",
    );
    expect(
        "??__FFLASH_TEMP_FILENAME@sandboxing@mozilla@@YAXXZ",
        "void __cdecl mozilla::sandboxing::FLASH_TEMP_FILENAME::`dynamic atexit destructor'(void)",
    );
    expect(
        "??__J?1??f@@YAAAUS@@XZ@5BB@",
        "`struct S & __cdecl f(void)'::`2'::`local static thread guard'{17}",
    );
    expect(
        "??__J?@??f@@YAAAUS@@XZ@5BB@",
        "`struct S & __cdecl f(void)'::`0'::`local static thread guard'{17}",
    );
    expect(
        "??__J?A@??f@@YAAAUS@@XZ@5BB@",
        "`struct S & __cdecl f(void)'::`anonymous namespace'::`local static thread guard'{17}",
    );
    expect(
        "??__J?B@??f@@YAAAUS@@XZ@5BB@",
        "`struct S & __cdecl f(void)'::`1'::`local static thread guard'{17}",
    );
    expect(
        "??__J?@??f@@YAAAUS@@XZ@5BB@",
        "`struct S & __cdecl f(void)'::`0'::`local static thread guard'{17}",
    );
}

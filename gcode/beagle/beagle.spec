Summary:     The Beagle Search Infrastructure
Name:        beagle
Version:     0.3.9
Release:     1
License:     LGPL
Group:       Applications/Development
Source:      %{name}-%{version}.tar.gz
BuildRoot:   /var/tmp/%{name}-%{version}-root
BuildPrereq: evolution-sharp, mono-core
Requires:    evolution-sharp >= 0.4, mono-core
Prefix:	     /opt/gnome

%description
A general infrastructure for making your data easy to find. 

%prep
%setup -q

%build
./configure --prefix=%{_prefix} \
	--localstatedir=/var/lib \
	--datadir=%{_prefix}/share \
	--sysconfdir=%{_sysconfdir}
make

%install
rm -rf $RPM_BUILD_ROOT
MAKE=${MAKE:-make}
DESTDIR=${DESTDIR:-"$RPM_BUILD_ROOT"}
case "${RPM_COMMAND:-all}" in
install|all)
        make install DESTDIR=${DESTDIR}
        ;;
esac

%clean
rm -rf $RPM_BUILD_ROOT

%post

%files
%defattr(-,root,root)
%doc COPYING README
%{_prefix}/lib/beagle/*
%{_prefix}/bin/beagle*
%{_prefix}/bin/best
%{_prefix}/bin/searchomatic
%{_prefix}/lib/epiphany/extensions/*beagle*
%{_prefix}/lib/*beagle*
%{_prefix}/lib/pkgconfig/beagle*
%{_prefix}/man/man1/beagle*
%{_sysconfdir}/cron.daily/beagle-*
%{_sysconfdir}/beagle/*

%changelog
* Thu Aug 27 2004 Nat Friedman <nat@novell.com>
- initial packaging of 0.0.3

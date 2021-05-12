Name:           bs
Version:        %{_release}
Release:        %{_version}
Summary:        BS App
License:        aamarnathkp
Source:         bs-1.0.tar.gz
Packager:       aamarnathkp

%description
BS App Server

%define _unpackaged_files_terminate_build 0

%pre
id -u bsapp &>/dev/null || /usr/sbin/useradd bsapp

%post
chown bsapp:users /opt/bs_app
mkdir -p /opt/bs_app/var
chown bsapp:users /opt/bs_app/var
echo "export BS_HOME=/opt/bs_app" >> /home/bsapp/.bashrc
cp /opt/bs_app/bs/bin/bs-app.service /etc/systemd/system/
echo "export PATH=\"/opt/bs_app/bs/bin:\$PATH\"" >> /home/bsapp/.bashrc
echo "%{_release}.%{_version}" > /opt/bs_app/release
systemctl enable bs-app.service


%prep

%setup -c bs-app-1.0

%install
(mkdir -p $RPM_BUILD_ROOT/opt/bs_app/bs; cd $RPM_BUILD_ROOT/opt/bs_app/bs; tar -xvf $RPM_BUILD_ROOT/../../SOURCES/bs-1.0.tar.gz;)

%postun
systemctl stop bs-app.service
systemctl disable bs-app.service
rm /etc/systemd/system/bs-app.service
rm -rf /opt/bs_app/bs/
sed -i '/BS_HOME/d' /home/bsapp/.bashrc
sed -i '/bs/d' /home/bsapp/.bashrc
rm /opt/bs_app/release

%files
%defattr(-, bsapp,root,-)
"/opt/bs_app/"

# Hackathon ACP Summer school 2019

Our entry for the ACP Summer School 2019 hackathon. 

## Problem description

The goal of our solution is to solve a roadwork planning problem.


## Description

Basic description of our solution here

## To get the OscaR libs:
save this as your maven configuration file (on linux, the default is something like ~/.m2/settings.xml)
```xml
<?xml version="1.0" encoding="UTF-8"?>
<settings xmlns="http://maven.apache.org/SETTINGS/1.0.0"
          xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
          xsi:schemaLocation="http://maven.apache.org/SETTINGS/1.0.0 http://maven.apache.org/xsd/settings-1.0.0.xsd">
    <profiles>
        <profile>
            <repositories>
                <repository>
                    <snapshots>
                        <enabled>false</enabled>
                    </snapshots>
                    <id>central</id>
                    <name>libs-release</name>
                    <url>http://artifactory.info.ucl.ac.be/artifactory/libs-release</url>
                </repository>
                <repository>
                    <snapshots />
                    <id>snapshots</id>
                    <name>libs-snapshot</name>
                    <url>http://artifactory.info.ucl.ac.be/artifactory/libs-snapshot</url>
                </repository>
            </repositories>
            <pluginRepositories>
                <pluginRepository>
                    <snapshots>
                        <enabled>false</enabled>
                    </snapshots>
                    <id>central</id>
                    <name>plugins-release</name>
                    <url>http://artifactory.info.ucl.ac.be/artifactory/plugins-release</url>
                </pluginRepository>
                <pluginRepository>
                    <snapshots />
                    <id>snapshots</id>
                    <name>plugins-snapshot</name>
                    <url>http://artifactory.info.ucl.ac.be/artifactory/plugins-snapshot</url>
                </pluginRepository>
            </pluginRepositories>
            <id>artifactory</id>
        </profile>
    </profiles>
    <activeProfiles>
        <activeProfile>artifactory</activeProfile>
    </activeProfiles>
</settings>
```

## Participants

Quentin Fabry

Julien Girard-Satabin

Alex Mattenet 

Adriana Pacheco

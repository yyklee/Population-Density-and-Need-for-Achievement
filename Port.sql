CREATE DATABASE YProject;
USE YProject;

SELECT *
FROM CovidDeaths
ORDER BY 3,4;

-- Total cases vs. Total deaths:

SELECT Location, date, (total_deaths/total_cases)*100 as DeathPercentage
FROM CovidDeaths
WHERE location like '%states%'
ORDER BY 1, 2;

-- Total cases vs Population:

SELECT Location, date, (total_cases/population)*100 as CovidPercentage
FROM CovidDeaths
GROUP BY Location
ORDER BY 1,2;

-- Looking at Countries with Highest Infection Rate compare to Population

SELECT Location, MAX(total_cases) as HighestInfectionCount, Max((total_cases/population))*100 as PercentInfect
FROM CovidDeaths
GROUP BY Location, Population
ORDER BY PercentInfect;


-- Showing continents with highest death count

SELECT continent, MAX(Total_deaths) as TotalDeathCount, Max((Total_deaths/population))*100 as PercentInfect
FROM CovidDeaths
WHERE continent is not null
GROUP BY continent 
ORDER BY PercentInfect;

-- GLOBAL NUMBERS

Select SUM(new_cases) as total_cases, SUM(CONVERT(INT, new_deaths)) as total_deaths, SUM(new_deaths)/Sum(cast(new_cases as int))*100
FROM CovidDeaths
WHERE continent is not null
-- GROUP BY date;


-- Looking at Total Population vs Vaccinations.
SELECT dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations, SUM(vac.new_vaccinations) OVER (
FROM CovidDeaths dea
join CovidVaccinations vac
on dea.location = vac.location
and dea.date = vac.date
order by 2, 3;



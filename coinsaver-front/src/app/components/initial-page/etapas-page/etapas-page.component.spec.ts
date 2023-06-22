import { ComponentFixture, TestBed } from '@angular/core/testing';

import { EtapasPageComponent } from './etapas-page.component';

describe('EtapasPageComponent', () => {
  let component: EtapasPageComponent;
  let fixture: ComponentFixture<EtapasPageComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ EtapasPageComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(EtapasPageComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
